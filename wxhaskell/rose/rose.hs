module Main where
import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    f <- frame [text := "Roses Buttons"]
    button_color <- button f [
        text := "Funny button",
        fontSize := 20,
        clientSize := sz 200 50,
        color := blue,
        textBgcolor := red,
        bgcolor := red
        ]
    button_onoff <- button f [
        text := "On"
        ]
    bitmapButton_rose <- bitmapButton f [
        picture := "/home/fabian/Documents/programming/haskell/gui/wxhaskell/rose/rose1.png",
        clientSize := sz 100 100
        ]
    button_quit <- button f [text := "Quit", on command := close f]
    set button_color [on command := funnyButton button_color f]
    set bitmapButton_rose [
        on command := enableDisable button_onoff f
        ]
    set f [layout :=
        grid 5 5 [
            [row 5 [
                widget button_color,
                widget bitmapButton_rose,
                widget button_onoff,
                widget button_quit]
                ],
            [label "Presiona la rosa para encender o apagar el boton"]
            ]
        ]
    return ()
    where
        funnyButton w f = do
            --let colors = [red, blue, green, yellow]
            set f [bgcolor := yellow]
        switch b f is_enabled
            | is_enabled = do set b [text := "OFF", enabled := False]
            | not is_enabled = do set b [text := "ON", enabled := True]
        enableDisable b f = do
            is_enabled <- get b enabled
            switch b f is_enabled
          

