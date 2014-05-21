module Main where
import Graphics.UI.WX
import Data.List

nReinas :: Int -> [[Int]]
nReinas n = [xs | xs <- permutations [1..n], esTableroValido xs]


esTableroValido :: [Int] -> Bool
esTableroValido [] = True
esTableroValido (x:xs) = esTableroValido xs && noAtaca x xs

noAtaca :: Int -> [Int] -> Bool
noAtaca x xs =
    all (\x' -> abs (x - x') /= (pos x' xs)) xs
    where
        pos e lista = 1 + head (elemIndices e lista)


main :: IO ()
main = start gui

gui :: IO ()
gui = do
    let side_square = 88 --un escaque/casilla mide 88px de lado
        side_piece = 64
        file_board = "board.jpg"
        file_queen = "queen.png"
        fake_positions = nReinas 8
        real_positions = [zip [1..length fake_positions] x | x <- fake_positions]

    f <- frame [text := "nQueen"] 
    canvas <- window f []

    bm_board <- bitmapCreateFromFile file_board
    bm_queen <- bitmapCreateFromFile file_queen
    bmsize_board <- get bm_board size
    set canvas [clientSize:= bmsize_board]
    button_next <- button f [text := "Next Solution"]
    set button_next [on command := do
        set canvas [on paint := paintBoard bm_board bm_queen side_square 
            side_piece real_positions button_next canvas]
        repaint canvas
        return ()
        ]
    set f [layout := 
            row 5 [vfill $ widget button_next, fill $ widget canvas]
        ]
    return ()
    where
        paintQueen bm side_square distance [] dc v = do
            return ()
        paintQueen bm side_square distance (p:ps) dc v = do
            let x = fst p
                y = snd p
                point = pt ((x - 1) * side_square + distance) 
                    ((y - 1) * side_square + distance)
            drawBitmap dc bm point False []
            paintQueen bm side_square distance ps dc v

        --paintBoard bm_board bm_queen side_square side_piece [] dc = do
        --    return ()

        paintBoard bm_board bm_queen side_square side_piece []
         mybutton mycanvas dc v = do
            drawText dc "FIN" (pt 350 350) []
            return ()
        paintBoard bm_board bm_queen side_square side_piece positions@(p:ps)
         mybutton mycanvas dc v = do
            let distance = (side_square - side_piece) `div` 2
            putStrLn $ show p
            drawBitmap dc bm_board pointZero False []
            paintQueen bm_queen side_square distance p dc v
            set mybutton [on command := do
                set mycanvas [ on paint := paintBoard bm_board bm_queen
                    side_square side_piece ps mybutton mycanvas]
                repaint mycanvas
                return () 
                ]
            return ()
            --drawBitmap dc bm_queen (pt 150 50) False []

