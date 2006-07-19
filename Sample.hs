
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event


data Gui = Gui {
    window :: Window,
    txt :: TextBox,
    sb :: StatusBar,
--    open :: ToolButton,
--    save :: ToolButton,
--    close :: ToolButton,
    
    document :: Var Bool,
    modified :: Var Bool,
    filename :: Var (Maybe String)
    }



main = do
    initPropLang
    window <- getWindow "sample.glade" "wndMain"
    txt <- getTextBox window "txt"
    sb <- getStatusBar window "sb"
    
    e <- newEventName "Sample.test"
    e += putStrLn "event fired"
    raise e
    
    document <- newVar False
    modified <- newVar False
    filename <- newVar Nothing
    
    let gui = Gui{window=window, sb=sb, txt=txt,
                  document=document, modified=modified, filename=filename}
    
    
    window!text =<= txt!text
    txt!text -< "hi everyone :)"
    
    sb!text =< with1 (txt!text) (\x ->
        "Word count: " ++ show (length $ words x))

    showWindowMain window
