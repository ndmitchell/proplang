
module PropLang.Gtk(
    text, (!),
    initPropLang, mainPropLang,
    Window, getWindow, showWindow, showWindowMain,
    TextBox, getTextBox,
    StatusBar, getStatusBar,
    --ToolButton, getToolButton,
    ) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk hiding (Action, Window)
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.Multiline.TextView as GtkMultiline

import PropLang.Variable
import PropLang.Value
import PropLang.Event

import Data.IORef
import Foreign.C.Types


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
instance TextProvider TextBox where; text = textboxText
instance TextProvider StatusBar where; text = statusbarText


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
    windowText :: Var String
    }


getWindow :: FilePath -> String -> IO Window
getWindow file name = do
        dialogXmlM <- xmlNew file
        let dialogXml = case dialogXmlM of
                (Just dialogXml) -> dialogXml
                Nothing -> error $ "Can't find the glade file \"" ++ file ++ "\""

        wnd <- xmlGetWidget dialogXml castToWindow name
        windowText <- gtkProp ("gtk.window[" ++ name ++ "]")
                              (windowSetTitle wnd)
                              (windowGetTitle wnd)
        return $ Window dialogXml wnd windowText


showWindow :: Window -> IO ()
showWindow wnd = widgetShowAll $ window wnd



data TextBox = TextBox {
    textbox :: Gtk.TextView, textboxText :: Var String
    }


getTextBox :: Window -> String -> IO TextBox
getTextBox window ctrl = do
    txt <- xmlGetWidget (xml window) castToTextView ctrl
    buf <- textViewGetBuffer txt
    windowText <- gtkPropEvent ("gtk.textbox[" ++ ctrl ++ "]")
                               (afterBufferChanged buf)
                               (textBufferSetText buf)
                               (textBufferGet buf)
    return $ TextBox txt windowText

    where
        textBufferGet buf = do
            strt <- textBufferGetStartIter buf
            end <- textBufferGetEndIter buf
            textBufferGetText buf strt end False


ignore2 :: ((a -> b -> IO ()) -> IO ans) -> IO () -> IO ans
ignore2 app f = app (\a b -> f)


data StatusBar = StatusBar {
    statusbar :: Gtk.Statusbar, statusbarText :: Var String,
    context :: CUInt, statusbarValue :: IORef String
    }

getStatusBar :: Window -> String -> IO StatusBar
getStatusBar window ctrl = do
    sb <- xmlGetWidget (xml window) (castToStatusbar) ctrl
    context <- statusbarGetContextId sb ""
    val <- newIORef ""
    statusbarText <- gtkProp ("gtk.statusbar[" ++ ctrl ++ "]")
                             (statusBarSet sb val context)
                             (readIORef val)
    return $ StatusBar sb statusbarText context val
    
    where
        statusBarSet sb val context x = do
            writeIORef val x
            statusbarPop sb context
            statusbarPush sb context x
            return ()

    
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
