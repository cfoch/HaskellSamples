width :: Int
width = 5

height :: Int
height = 5

type Position = (Int, Int)
type Board = [Position]

{-
glider :: Board
glider = [
    (4, 2),
    (2, 3),
    (4, 3),
    (3, 4),
    (4, 4)
]
-}

goto :: Position -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
