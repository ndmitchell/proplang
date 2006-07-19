
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event


data Gui = Gui {
    window :: Window,
    txt :: TextBox,
    sb :: StatusBar,
    new :: ToolButton,
    open :: ToolButton,
    save :: ToolButton,
    close :: ToolButton,
    
    document :: Var Bool,
    modified :: Var Bool,
    filename :: Var (Maybe String)
    }



main = do
    initPropLang
    window <- getWindow "sample.glade" "wndMain"
    txt <- getTextBox window "txt"
    sb <- getStatusBar window "sb"
    new <- getToolButton window "tbNew"
    open <- getToolButton window "tbOpen"
    save <- getToolButton window "tbSave"
    close <- getToolButton window "tbClose"
    
    e <- newEventName "Sample.test"
    e += putStrLn "event fired"
    raise e
    
    -- is a document open
    document <- newVar False
    
    -- is a document modified
    modified <- newVar False
    
    -- the filename of the document, if its been saved
    filename <- newVar Nothing
    
--    let gui = Gui{window=window, sb=sb, txt=txt,
--                  document=document, modified=modified, filename=filename}
    
    
    -- window!text =<= txt!text
    -- txt!text -< "hi everyone :)"
    
    txt!enabled =<= document
    new!enabled =< with1 document not 
    close!enabled =<= document
    save!enabled =< with2 document modified (&&)
    
    
    sb!text =< with1 (txt!text) (\x ->
        "Word count: " ++ show (length $ words x))

    showWindowMain window
