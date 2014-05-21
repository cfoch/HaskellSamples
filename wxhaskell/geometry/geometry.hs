module Main where
import Graphics.UI.WX

main :: IO ()
main = start gui

gui = do
    let height = 600
        width = 630
        shapes_list = [
            ("Square", paintSquare),
            ("Triangle", paintTriangle),
            ("Circle", paintCircle)
            ]
        shapes = [ fst x | x <- shapes_list]
        paintShapeList = [ snd x | x <- shapes_list]
    f <- frame [text := "Geometry Shapes"]
    radioBox_shapes <- radioBox f Vertical shapes [text := "Shapes"]
    window_canvas <- window f []
    set radioBox_shapes [on select := setShape window_canvas radioBox_shapes paintShapeList]
    set f [layout := minsize (sz height width) $
        fill $ row 5 [
            vfill $ widget radioBox_shapes,
            fill $ widget window_canvas
            ]
        ]
    return ()
    where
        defaultPenWidth = 4

        paintCircle point dc' viewarea' = do
            let radius = 120 --arbitrario
            circle dc' point radius [penColor := red, penWidth := defaultPenWidth]

        paintSquare point dc' viewarea' = do
            let side = 250 --arbitrario
                x = pointX point - side `div` 2 --esquina del cuadrado
                y = pointY point - side `div` 2
                points = [
                    pt x y,
                    pt (x + side) y,
                    pt (x + side) (y + side),
                    pt x (y + side)
                    ]
            polygon dc' points [penColor := blue, penWidth := defaultPenWidth]
        
        paintTriangle point dc' viewarea' = do --triangulo equilatero
            let side = 250
                aux = round $ (fromIntegral side / 2) * (sqrt 3) / 3--distancia del centro a la base
                --aux = 217 `div` 3
                x = pointX point --punta superior del triangulo
                y = pointY point - 2 * aux
                points = [
                    pt x y,
                    pt (x + (side `div` 2)) (y + 3 * aux),
                    pt (x - (side `div` 2)) (y + 3 * aux)
                    ]
            putStrLn $ show (x, y)
            polygon dc' points [penColor := green, penWidth := defaultPenWidth]

        setShape window' radiobox' paintShapeList = do
            i <- get radiobox' selection
            wsize <- get window' clientSize
            let middlePoint = pt ((sizeW wsize) `div` 2) ((sizeH wsize) `div` 2)
            set window' [on paint := (paintShapeList !! i) middlePoint]
            repaint window'
            return ()


