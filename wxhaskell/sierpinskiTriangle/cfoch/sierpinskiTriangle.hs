module Main where
import Graphics.UI.WX

main :: IO ()
main = start gui

gui = do
    let width = 500
        height = 500
    f <- frame [text := "Sierpinski Triangle"]
    canvas <- window f [clientSize := sz width height, on paint := sierpinski 2 (pt 20 20) 300] 
    return ()
    where
        x = 20
        y = 20
        defaultPenWidth = 2
        paintTriangle (Point x y) side dc' viewarea' = do
            let points = [
                    pt x y,
                    pt (x + side) y,
                    pt (x + side) (y + side)
                    ]
            polygon dc' points [penWidth := defaultPenWidth]
        sierpinski n point' side'
            | n == 1 = do 
                paintTriangle point' side'
            | n > 1 = do
                let side = side' `div` 2
                sierpinski (n - 1) (pt (pointX point') side) side
                sierpinski (n - 1) (pt (pointX point') (3 * side)) side
                --sierpinski (n - 1) (pt (pointX point') side) side
