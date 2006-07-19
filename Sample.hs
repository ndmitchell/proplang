
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe


data Gui = Gui {
    window :: Window,
    txt :: TextBox,
    sb :: StatusBar,
    new :: ToolButton,
    open :: ToolButton,
    save :: ToolButton,
    saveas :: ToolButton,
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
    saveas <- getToolButton window "tbSaveAs"
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
    
    let gui = Gui{window=window, sb=sb, txt=txt,
                  new=new, open=open, save=save, saveas=saveas, close=close,
                  document=document, modified=modified, filename=filename}
    
    txt!enabled =<= document
    new!enabled =< with1 document not 
    close!enabled =<= document
    saveas!enabled =<= document
    save!enabled =< with3 document modified filename
        (\d m f -> d && m && isJust f)
    
    new!onClicked += newDocument gui
    save!onClicked += saveDocument gui
    close!onClicked += closeDocument gui
    open!onClicked += openDocument gui
    
    txt!text += (modified -< True)
    
    window!text =< with3 document modified filename (\d m f -> 
        "TextEditor" ++
        (if d then " - " ++ maybe "<untitled>" id f else "") ++
        (if m then " *" else "")
        )
    
    sb!text =< with1 (txt!text) (\x ->
        "Word count: " ++ show (length $ words x))

    showWindowMain window


newDocument :: Gui -> IO ()
newDocument gui@Gui{document=document, modified=modified} = do
    b <- shutDocument gui
    when b $ do
        document -< True


saveDocument :: Gui -> IO ()
saveDocument gui@Gui{filename=filename, modified=modified} = do
    return ()
    


closeDocument :: Gui -> IO ()
closeDocument gui = do
    shutDocument gui
    return ()


openDocument :: Gui -> IO ()
openDocument gui = return () -- do
    


-- return True if the document was shut
-- False if the user cancelled, i.e. want to save it
shutDocument :: Gui -> IO Bool
shutDocument gui@Gui{modified=modified, txt=txt, filename=filename, document=document} = do
    moded <- getVar modified
    if moded then do
        return False
     else do
        txt!text -< ""
        filename -< Nothing
        document -< False
        modified -< False
        return True

{-

promptSave :: Gui -> IO Bool



-- make sure there is a filename
-- so that you can save to it
ensureFilename :: Gui -> IO Bool




askFilename :: Gui -> IO Bool

-}