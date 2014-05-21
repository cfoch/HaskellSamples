module Main where
import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    f <- frame [text := "Factorial"]
    label_number <- staticText f [text := "Number:"]
    spinCtrl_number <- spinCtrl f 0 100 []
    label_result <- staticText f [text := "Result:"]
    entry_result <- entry f []
    button_calc <- button f [
        text := "Calulate!"
        ,on command := updateResult spinCtrl_number entry_result f
        ]
    button_quit <- button f [
        text := "Quit",
        on command := close f
        ]
    set f [layout :=
        fill $ grid 5 5 [
            map fill [widget label_number, widget spinCtrl_number],
            map fill [widget label_result, widget entry_result],
            map fill [widget button_calc, widget button_quit]
            ]
        ]
    return ()
    where
        factorial n
            | n == 0 = 1
            | otherwise = foldl1 (\acc x -> acc * x) [1..n]
        updateResult numberW resultW f = do
            n <- get numberW selection
            set resultW [text := show $ factorial n]


    
