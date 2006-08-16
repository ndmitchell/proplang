
import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Control.Monad
import Data.Maybe

import Graphics.UI.Gtk.Windows.Dialog
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.Selectors.FileChooserDialog


data Gui = Gui {
    window :: Window,
    txt :: TextView,

    document :: Var Bool,
    modified :: Var Bool,
    filename :: Var (Maybe String),
    lasttxt  :: Var String
    }



main = do
    initPropLang
    window <- getWindow "sample.glade" "wndMain"
    
    let f x = getCtrl window x
        txt = f "txt"
        sb :: StatusBar
        sb = f "sb"
        new :: ToolButton
        new = f "tbNew"
        open :: ToolButton
        open = f "tbOpen"
        save = f "tbSave"
        save :: ToolButton
        saveas :: ToolButton
        saveas = f "tbSaveAs"
        close :: ToolButton
        close = f "tbClose"
        revert :: ToolButton
        revert = f "tbRevert"
    
    e <- newEventName "Sample.test"
    e += putStrLn "event fired"
    raise e
    
    -- is a document open
    document <- newVar False
    
    -- the last saved text
    lasttxt <- newVar ""

    -- is a document modified
    modified <- newVar False
    
    -- the filename of the document, if its been saved
    filename <- newVar Nothing
    
    let gui = Gui{window=window, txt=txt,
                  document=document, modified=modified, filename=filename, lasttxt=lasttxt}
    
    txt!enabled =<= document
    new!enabled =< with1 document not 
    saveas!enabled =< with2 document modified (&&)
    save!enabled =< with2 document modified (&&)
    close!enabled =<= document
    revert!enabled =<= modified
    
    new!onClicked += newDocument gui
    save!onClicked += saveDocument gui
    saveas!onClicked += saveAsDocument gui
    close!onClicked += closeDocument gui
    open!onClicked += openDocument gui
    revert!onClicked += (txt!text -<- lasttxt)
    
    modified =< with2 (txt!text) (lasttxt) (/=)
    
    window!text =< with3 document modified filename (\d m f -> 
        "TextEditor" ++
        (if d then " - " ++ maybe "<untitled>" id f else "") ++
        (if m then " *" else "")
        )
    
    sb!text =< with1 (txt!text) (\x ->
        "Word count: " ++ show (length $ words x))

    showWindowMain window


newDocument :: Gui -> IO ()
newDocument gui@Gui{document=document} = do
    b <- shutDocument gui
    when b $ do
        document -< True

readFromFile :: Gui -> String -> IO ()
readFromFile gui@Gui{lasttxt=lasttxt, txt=txt} filename = do
    content <- readFile filename
    (txt!text) -< content
    lasttxt    -< content

saveToFile :: Gui -> String -> IO ()
saveToFile gui@Gui{lasttxt=lasttxt, txt=txt} filename = do
    text <- getVar (txt!text)
    writeFile filename text
    lasttxt -< text

saveDocument :: Gui -> IO ()
saveDocument gui@Gui{filename=filename} = do
    fname <- getVar filename
    case fname of
        Nothing -> saveAsDocument gui
        Just fn -> saveToFile gui fn
    return ()

saveAsDocument :: Gui -> IO ()
saveAsDocument gui@Gui{filename=filename} = do
    fname <- getVar filename
    dlg <- fileChooserDialogNew
            (Just "Save Document")
            (Just  $ getWindowRaw $ window gui)
            FileChooserActionSave
            [ ("Save",ResponseOk) , ("Cancel",ResponseCancel) ]
    case fname of
        Nothing -> return ()
        Just fn -> fileChooserSetCurrentName dlg fn
    res <- dialogRun dlg
    case res of
        ResponseOk     -> do Just fn <- fileChooserGetFilename dlg -- needs error checking
                             filename -< Just fn
                             saveToFile gui fn
        ResponseCancel -> return ()
    widgetDestroy dlg

closeDocument :: Gui -> IO ()
closeDocument gui = do
    shutDocument gui
    return ()


openDocument :: Gui -> IO ()
openDocument gui = do
    b <- shutDocument gui 
    if b then do
        fname <- getVar (filename gui)
        dlg <- fileChooserDialogNew
                (Just "Open Document")
                (Just  $ getWindowRaw $ window gui)
                FileChooserActionOpen
                [ ("Open",ResponseOk) , ("Cancel",ResponseCancel) ]
        case fname of
            Nothing -> return ()
            Just fn -> fileChooserSetCurrentName dlg fn
        res <- dialogRun dlg
        case res of
            ResponseOk     -> do Just fn <- fileChooserGetFilename dlg -- needs error checking
                                 (filename gui) -< Just fn
                                 readFromFile gui fn
                                 (document gui) -< True
            ResponseCancel -> return ()
        widgetDestroy dlg
      else return ()


-- return True if the document was shut
-- False if the user cancelled, i.e. want to save it
shutDocument :: Gui -> IO Bool
shutDocument gui@Gui{modified=modified, txt=txt, lasttxt=lasttxt, filename=filename, document=document} = do
    doc <- getVar document
    moded <- getVar modified
    ok <- if doc then
             if moded then promptSave gui else return True
          else return True
    if ok then do
        txt!text -< ""
        lasttxt  -< ""
        filename -< Nothing
        document -< False
        return True
      else
        return False


promptSave :: Gui -> IO Bool
promptSave gui = do
    dlg <- dialogNew
    label <- labelNew (Just "Do you want to save your changes?")
    upper <- dialogGetUpper dlg
    containerAdd upper label
    dialogAddButton dlg "Yes" ResponseYes
    dialogAddButton dlg "No" ResponseNo
    dialogAddButton dlg "Cancel" ResponseCancel
    dialogSetDefaultResponse dlg ResponseYes
    widgetShow label
    res <- dialogRun dlg
    widgetDestroy dlg
    case res of
        ResponseYes    -> do saveDocument gui
                             return True 
        ResponseNo     -> return True
        ResponseCancel -> return False

{-
-- make sure there is a filename
-- so that you can save to it
ensureFilename :: Gui -> IO Bool




askFilename :: Gui -> IO Bool

-}
