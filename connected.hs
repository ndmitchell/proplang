import PropLang.Gtk
import PropLang.Variable
import PropLang.Event

import Char

caesar n = map (caesar' n)
caesar' n c | isAlpha c = chr $ (((ord(c)-ord('a'))+n) `mod` 26) + ord('a')
            | otherwise = c

rot13 = caesar 13

main = do
    initPropLang
    window <- getWindow "connected.glade" "window1"
    
    let f x = getTextEntry window x
        entry1 = f "entry1"
        entry2 = f "entry2"
        entry3 = f "entry3"
        entry4 = f "entry4"
    
    entry1!text =<>= entry2!text

    tie (entry2!text) (entry3!text) rot13 rot13
    tie (entry2!text) (entry4!text) (caesar 3) (caesar 23)

    showWindowMain window
