
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event


data Gui = Gui {
    window :: Window,
    txt :: TextBox,
--    sb :: StatusBar,
--    open :: ToolButton,
--    save :: ToolButton,
--    close :: ToolButton,
    
    modified :: Var Bool,
    filename :: Var (Maybe String)
    }



main = do
    initPropLang
    window <- getWindow "sample.glade" "wndMain"
    txt <- getTextBox window "txt"
    
    e <- newEventName "Sample.test"
    e += putStrLn "event fired"
    raise e
    
    modified <- newVar False
    filename <- newVar Nothing
    
    let gui = Gui{window=window, txt=txt, modified=modified, filename=filename}
    
    
    window!text =<= txt!text
    txt!text -< "hi everyone :)"

    showWindowMain window
