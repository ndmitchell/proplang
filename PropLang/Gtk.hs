-----------------------------------------------------------------------------
-- |
-- Module      :  Gtk.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  not portable
--
-- Bindings to Gtk
--
-----------------------------------------------------------------------------

module PropLang.Gtk(
    (!),
    text, enabled, menu, onClicked, onActivated,
    initPropLang, mainPropLang,
    Window, getWindow, showWindow, showWindowMain, getWindowRaw,
    ComboBox,
    MenuItem, getMenuItem,
    TextView, getTextView, getTextViewRaw,
    textviewBuffer,
    StatusBar, getStatusBar,
    ToolButton, getToolButton,
    TextEntry, getTextEntry,
    getCtrl,
    
    -- hacks!
    onEnterKey
    ) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk hiding (Action, Window, ComboBox, MenuItem, TextView, ToolButton, Event, onClicked, onChanged)
import Graphics.UI.Gtk.Glade
import System.Glib

import PropLang.Variable
import PropLang.Value
import PropLang.Event

import Data.IORef
import Data.Maybe
import Data.List
import Foreign.C.Types
import Control.Exception
import Control.Concurrent
import Control.Monad

debug = putStrLn

-- | Initialisation functions from GTK
initPropLang :: IO HandlerId
initPropLang = do if rtsSupportsBoundThreads
                      then error "Don't link with -theaded, Gtk won't work"
                      else do
                             initGUI
                             timeoutAddFull (yield >> return True) priorityDefaultIdle 50

-- | Start Gtk
mainPropLang = mainGUI

-- | Start PropLang
showWindowMain wnd = do
    window wnd `onDestroy` mainQuit
    showWindow wnd
    mainPropLang


-- Property stuff

infixl 9 !

-- | Access widget properties
(!) :: a -> (a -> b) -> b
object ! prop = prop object

-- |
class TextProvider a where; text :: a -> Var String
instance TextProvider Window where; text = windowText
instance TextProvider ComboBox where; text = comboboxText
instance TextProvider TextView where; text = textviewText
instance TextProvider StatusBar where; text = statusbarText
instance TextProvider TextEntry where; text = textentryText

-- |
class EnabledProvider a where; enabled :: a -> Var Bool
instance EnabledProvider TextView where; enabled = textviewEnabled
instance EnabledProvider ToolButton where; enabled = toolbuttonEnabled

-- |
class MenuProvider a where; menu :: a -> Var Gtk.Menu
instance MenuProvider MenuItem where; menu = menuitemSubmenu

-- |
class OnClickedProvider a where; onClicked :: a -> Event
instance OnClickedProvider ToolButton where; onClicked = toolbuttonOnClicked

-- |
class OnChangedProvider a where; onChanged :: a -> Event
instance OnChangedProvider ComboBox where; onChanged = comboboxOnChanged

-- |
class OnActivatedProvider a where; onActivated :: a -> Event
instance OnActivatedProvider MenuItem where; onActivated = menuitemOnActivated

-- Helper stuff

gtkProp :: String -> (s -> IO ()) -> (IO s) -> IO (Var s)
gtkProp name set get = newVarWithName name f
    where
        f e = return $ Value (set2 e) get
        set2 e x = do set x
                      raise e

gtkPropEvent ::  String -> (IO () -> IO any_) -> (String -> IO ()) -> (IO String) -> IO (Var String)
gtkPropEvent name reg set get = newVarWithName name f
    where
        f e = do reg (raise e)
                 return $ Value set' get'
              where set' s = do old <- get
	                        debug $ name++": want to change "++(show old)++" to "++(show s)
 				if old /= s then do
					blockEvent e
					set ""
					unblockEvent e
					set s
				 else debug "(not happening)"
	            get'   = do s <- get; debug (name++": Getting "++(show s)); return s
    -- we do not raise on set, as gtk does that anyway (tested with Entry)


-- | Window
data Window = Window {
    xml :: GladeXML, window :: Gtk.Window,
    children :: [(String,AWidget)],
    windowText :: Var String
    }

-- Hack, I guess.
getWindowRaw :: Window -> Gtk.Window
getWindowRaw = window

-- |
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
        c2 <- mapM f children
        return $ Window dialogXml wnd (catMaybes c2) windowText
    where
        f w = do
            name <- widgetGetName w
            if "Gtk" `isPrefixOf` name
                then return Nothing
                else do x <- liftWidget w
                        return $ Just (name,x)
                

-- |
showWindow :: Window -> IO ()
showWindow wnd = widgetShowAll $ window wnd

-- Widgets

data AWidget = AComboBox ComboBox
	     | AMenuItem MenuItem
	     | ATextView TextView
             | ATextEntry TextEntry
             | AStatusBar StatusBar
             | AToolButton ToolButton
             | AUnknown

liftWidget :: Widget -> IO AWidget
liftWidget x = do
    cb <- getWidgetMaybe castToComboBox x
    mi <- getWidgetMaybe castToMenuItem x
    tv <- getWidgetMaybe castToTextView x
    te <- getWidgetMaybe castToEntry x
    sb <- getWidgetMaybe castToStatusbar x
    tb <- getWidgetMaybe castToToolButton x
    case () of
	_ | isJust cb -> f AComboBox liftComboBox cb
        _ | isJust mi -> f AMenuItem liftMenuItem mi
        _ | isJust tv -> f ATextView liftTextView tv
        _ | isJust sb -> f AStatusBar liftStatusBar sb
        _ | isJust tb -> f AToolButton liftToolButton tb
        _ | isJust te -> f ATextEntry liftTextEntry te
        _ -> return AUnknown
    where
        f wrap conv (Just x) = do
            x2 <- conv x
            return $ wrap x2


getAWidget :: (AWidget -> a) -> Window -> String -> a
getAWidget f wnd name = case lookup name (children wnd) of
                            Nothing -> error $ "Widget not found: " ++ name
                            Just x -> f x


class GetCtrl a where
    getCtrl :: Window -> String -> a

instance GetCtrl ComboBox where ; getCtrl = getComboBox
instance GetCtrl MenuItem where ; getCtrl = getMenuItem
instance GetCtrl TextView where ; getCtrl = getTextView
instance GetCtrl StatusBar where ; getCtrl = getStatusBar
instance GetCtrl ToolButton where ; getCtrl = getToolButton
instance GetCtrl TextEntry where ; getCtrl = getTextEntry

--
-- | ComboBox
--

data ComboBox = ComboBox {
    comboBox :: Gtk.ComboBox,
    comboboxOnChanged :: Event,
    comboboxText :: Var String
    }

-- |
getComboBox :: Window -> String -> ComboBox
getComboBox window ctrl = getAWidget (\(AComboBox x) -> x) window ctrl

liftComboBox :: Gtk.ComboBox -> IO ComboBox
liftComboBox cb = do
    name <- widgetGetName cb
    cbChanged <- newEventName $ "gtk.combobox.changed [" ++ name ++ "]"
    -- Might be better just to use this widget as the source since
    -- it doesn't work as expected (i.e. appends) the other way
    comboboxText <- gtkPropEvent ("gtk.combobox.text[" ++ name ++ "]")
				  (afterChanged cb)
				  (comboBoxAppendText cb)
				  (return . maybe "" id =<< comboBoxGetActiveText cb)
    cb `Gtk.onChanged` raise cbChanged
    return $ ComboBox cb cbChanged comboboxText

--
-- | MenuItem
--

data MenuItem = MenuItem {
    menuItem :: Gtk.MenuItem,
    menuitemOnActivated :: Event,
    menuitemSubmenu :: Var Gtk.Menu
    }

-- |
getMenuItem :: Window -> String -> MenuItem
getMenuItem window ctrl = getAWidget (\(AMenuItem x) -> x) window ctrl

liftMenuItem :: Gtk.MenuItem -> IO MenuItem
liftMenuItem mi = do
    name <- widgetGetName mi
    miActivated <- newEventName $ "gtk.menuitem.activated [" ++ name ++ "]"
    menuitemSubmenu <- gtkProp ("gtk.menuitem.menu[" ++ name ++ "]")
			       (menuItemSetSubmenu mi)
			       (maybe menuNew (return . castToMenu) =<< menuItemGetSubmenu mi)
    mi `onActivateLeaf` raise miActivated
    return $ MenuItem mi miActivated menuitemSubmenu

--
-- | TextView
--

data TextView = TextView {
    textview :: Gtk.TextView,
    textviewText :: Var String, textviewEnabled :: Var Bool
    }


textviewBuffer :: TextView -> IO TextBuffer
textviewBuffer txt = textViewGetBuffer (textview txt)


getTextViewRaw :: TextView -> Gtk.TextView
getTextViewRaw txt = textview txt


-- |
getTextView :: Window -> String -> TextView
getTextView window ctrl = getAWidget (\(ATextView x) -> x) window ctrl

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

--
-- | TextEntry
--

data TextEntry = TextEntry {
    textentry :: Gtk.Entry,
    textentryText :: Var String
    }


-- |
getTextEntry :: Window -> String -> TextEntry
getTextEntry window ctrl = getAWidget (\(ATextEntry x) -> x) window ctrl

liftTextEntry :: Gtk.Entry -> IO TextEntry
liftTextEntry txt = do
    name <- widgetGetName txt
    textentryText <- gtkPropEvent ("gtk.textentry.text[" ++ name ++ "]")
                                  (afterEditableChanged txt)
                                  (entrySetText txt)
                                  (entryGetText txt)
    return $ TextEntry txt textentryText



--
-- | StatusBar
--

data StatusBar = StatusBar {
    statusbar :: Gtk.Statusbar, statusbarText :: Var String,
    context :: CUInt, statusbarValue :: IORef String
    }

-- |
getStatusBar :: Window -> String -> StatusBar
getStatusBar window ctrl = getAWidget (\(AStatusBar x) -> x) window ctrl

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

--
-- | ToolButton
--

data ToolButton = ToolButton {
    toolbutton :: Gtk.ToolButton,
    toolbuttonEnabled :: Var Bool,
    toolbuttonOnClicked :: Event
    }


-- |
getToolButton :: Window -> String -> ToolButton
getToolButton window ctrl = getAWidget (\(AToolButton x) -> x) window ctrl

liftToolButton :: Gtk.ToolButton -> IO ToolButton
liftToolButton tb = do
    name <- widgetGetName tb
    tbEnabled <- newEnabled tb ("gtk.toolbutton.enabled[" ++ name ++ "]")
    tbClicked <- newEventName $ "gtk.toolbutton.clicked[" ++ name ++ "]"
    tb `onToolButtonClicked` raise tbClicked
    return $ ToolButton tb tbEnabled tbClicked

--
-- Helper functions
--

widgetGetSensitivity :: WidgetClass self => self -> IO Bool
widgetGetSensitivity x = do
    y <- widgetGetState x
    return (y /= StateInsensitive)


newEnabled :: WidgetClass a => a -> String -> IO (Var Bool)
newEnabled x name = gtkProp name (widgetSetSensitivity x) (widgetGetSensitivity x)


ignore2 :: ((a -> b -> IO ()) -> IO ans) -> IO () -> IO ans
ignore2 app f = app (\a b -> f)



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
		containerGetChildren c
{-
            Just c -> do
                i <- newIORef []
                containerForeach c (f i)
                readIORef i
    where
        f i x = do
            r <- readIORef i
            writeIORef i (x:r)
-}          

getWidgetMaybe :: GObjectClass obj => (obj -> conc) -> obj -> IO (Maybe conc)
getWidgetMaybe cast o = 
    Control.Exception.catch
        (return $! Just $! cast o)
        (\e -> return Nothing)




-- short term hack
onEnterKey :: TextView -> IO () -> IO ()
onEnterKey txt act = do
        onKeyPress (textview txt) handle
        return ()
    where
        handle x = do
            case x of
                Key{eventKeyName = "Return"} -> act >> return True
                _ -> return False

    
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

