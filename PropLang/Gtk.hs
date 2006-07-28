
module PropLang.Gtk(
    (!),
    text, enabled, onClicked,
    initPropLang, mainPropLang,
    Window, getWindow, showWindow, showWindowMain,
    TextView, getTextView,
    StatusBar, getStatusBar,
    ToolButton, getToolButton,
    ) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk hiding (Action, Window, TextView, ToolButton, Event, onClicked)
import Graphics.UI.Gtk.Glade
import System.Glib

import PropLang.Variable
import PropLang.Value
import PropLang.Event

import Data.IORef
import Foreign.C.Types
import Control.Exception


-- Initialisation stuff from GTK

initPropLang = initGUI
mainPropLang = mainGUI

showWindowMain wnd = do
    window wnd `onDestroy` mainQuit
    showWindow wnd
    mainPropLang


-- Property stuff

(!) :: a -> (a -> b) -> b
object ! prop = prop object

class TextProvider a where; text :: a -> Var String
instance TextProvider Window where; text = windowText
instance TextProvider TextView where; text = textviewText
instance TextProvider StatusBar where; text = statusbarText

class EnabledProvider a where; enabled :: a -> Var Bool
instance EnabledProvider TextView where; enabled = textviewEnabled
instance EnabledProvider ToolButton where; enabled = toolbuttonEnabled

class OnClickedProvider a where; onClicked :: a -> Event
instance OnClickedProvider ToolButton where; onClicked = toolbuttonOnClicked

-- Helper stuff

gtkProp :: String -> (s -> IO ()) -> (IO s) -> IO (Var s)
gtkProp name set get = newVarWithName name f
    where
        f e = return $ Value (set2 e) get
        set2 e x = do set x
                      raise e

gtkPropEvent :: String -> (IO () -> IO any_) -> (s -> IO ()) -> (IO s) -> IO (Var s)
gtkPropEvent name reg set get = newVarWithName name f
    where
        f e = do reg (raise e)
                 return $ Value (set2 e) get
        set2 e x = do set x
                      raise e

-- Window

data Window = Window {
    xml :: GladeXML, window :: Gtk.Window,
    children :: [Widget],
    windowText :: Var String
    }


getWindow :: FilePath -> String -> IO Window
getWindow file name = do
        dialogXmlM <- xmlNew file
        let dialogXml = case dialogXmlM of
                (Just dialogXml) -> dialogXml
                Nothing -> error $ "Can't find the glade file \"" ++ file ++ "\""

        wnd <- xmlGetWidget dialogXml castToWindow name
        windowText <- gtkProp ("gtk.window.text[" ++ name ++ "]")
                              (windowSetTitle wnd)
                              (windowGetTitle wnd)
                              
        children <- getChildWindowsAll $ toWidget wnd
        return $ Window dialogXml wnd children windowText


showWindow :: Window -> IO ()
showWindow wnd = widgetShowAll $ window wnd



data TextView = TextView {
    textview :: Gtk.TextView,
    textviewText :: Var String, textviewEnabled :: Var Bool
    }


getTextView :: Window -> String -> IO TextView
getTextView window ctrl = do
    txt <- xmlGetWidget (xml window) castToTextView ctrl
    liftTextView txt

liftTextView :: Gtk.TextView -> IO TextView
liftTextView txt = do
    name <- widgetGetName txt
    buf <- textViewGetBuffer txt
    textviewText <- gtkPropEvent ("gtk.textview.text[" ++ name ++ "]")
                                (afterBufferChanged buf)
                                (textBufferSetText buf)
                                (textBufferGet buf)
    textviewEnabled <- newEnabled txt ("gtk.textview.enabled[" ++ name ++ "]")
    return $ TextView txt textviewText textviewEnabled

    where
        textBufferGet buf = do
            strt <- textBufferGetStartIter buf
            end <- textBufferGetEndIter buf
            textBufferGetText buf strt end False


widgetGetSensitivity :: WidgetClass self => self -> IO Bool
widgetGetSensitivity x = do
    y <- widgetGetState x
    return (y /= StateInsensitive)


newEnabled :: WidgetClass a => a -> String -> IO (Var Bool)
newEnabled x name = gtkProp name (widgetSetSensitivity x) (widgetGetSensitivity x)


ignore2 :: ((a -> b -> IO ()) -> IO ans) -> IO () -> IO ans
ignore2 app f = app (\a b -> f)


data StatusBar = StatusBar {
    statusbar :: Gtk.Statusbar, statusbarText :: Var String,
    context :: CUInt, statusbarValue :: IORef String
    }

getStatusBar :: Window -> String -> IO StatusBar
getStatusBar window ctrl = do
    sb <- xmlGetWidget (xml window) (castToStatusbar) ctrl
    liftStatusBar sb

liftStatusBar :: Gtk.Statusbar -> IO StatusBar
liftStatusBar sb = do
    name <- widgetGetName sb
    context <- statusbarGetContextId sb ""
    val <- newIORef ""
    statusbarText <- gtkProp ("gtk.statusbar[" ++ name ++ "]")
                             (statusBarSet sb val context)
                             (readIORef val)
    return $ StatusBar sb statusbarText context val
    
    where
        statusBarSet sb val context x = do
            writeIORef val x
            statusbarPop sb context
            statusbarPush sb context x
            return ()

data ToolButton = ToolButton {
    toolbutton :: Gtk.ToolButton,
    toolbuttonEnabled :: Var Bool,
    toolbuttonOnClicked :: Event
    }


getToolButton :: Window -> String -> IO ToolButton
getToolButton window ctrl = do
    tb <- xmlGetWidget (xml window) (castToToolButton) ctrl
    liftToolButton tb

liftToolButton :: Gtk.ToolButton -> IO ToolButton
liftToolButton tb = do
    name <- widgetGetName tb
    tbEnabled <- newEnabled tb ("gtk.toolbutton.enabled[" ++ name ++ "]")
    tbClicked <- newEventName $ "gtk.toolbutton.clicked[" ++ name ++ "]"
    tb `onToolButtonClicked` raise tbClicked
    return $ ToolButton tb tbEnabled tbClicked







-- window enumeration
getChildWindowsAll :: Widget -> IO [Widget]
getChildWindowsAll w = do
    child <- getChildWindows w
    child2 <- mapM getChildWindowsAll child
    return $ child ++ concat child2


getChildWindows :: Widget -> IO [Widget]
getChildWindows w = do
        c <- getWidgetMaybe castToContainer w
        case c of
            Nothing -> return []
            Just c -> do
                i <- newIORef []
                containerForeach c (f i)
                readIORef i
    where
        f i x = do
            r <- readIORef i
            writeIORef i (x:r)
            

getWidgetMaybe :: GObjectClass obj => (obj -> conc) -> obj -> IO (Maybe conc)
getWidgetMaybe cast o = 
    Control.Exception.catch
        (do c <- return $ cast o
            c `seq` return (Just c) -- return $! Just $! c
        )
        (\e -> return Nothing)


    
    {-
    
    type ContainerForeachCB = Widget -> IO ()
    
    
    
    cmdFilename <- xmlGetWidget dialogXml castToButton "cmdFilename"
    lblFilename <- xmlGetWidget dialogXml castToLabel "lblFilename"
    tvStack <- xmlGetWidget dialogXml castToTreeView "tvStack"
    lblStack <- xmlGetWidget dialogXml castToLabel "lblStack"
    tvStackData <- treeStoreNew [TMstring, TMstring]
    txtCover <- xmlGetWidget dialogXml castToTextView "txtCover"
    txtCoverData <- textViewGetBuffer txtCover
    let hatGui = HatGui
                    wndMain cmdFilename lblFilename
                    tvStack lblStack tvStackData
                    txtCover txtCoverData




-- they should all already be buffered
getTextBox :: Window -> String -> IO TextBox


data TextBox = TextBox {Gtk.TextBox, textVaraible}

instance TextProvider TextBox where
    text (TextBox a b) = b

-}




{-

varFromEntryText :: Entry -> IO (Var String)
varFromEntryText entry = do
    let val = Value (entrySetText entry) (entryGetText entry)
    var <- newVarValue val
    onInsertAtCursor entry (const $ fireNotify var)
    return var
-}
