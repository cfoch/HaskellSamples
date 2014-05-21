module Main where
import Graphics.UI.WX
-- Y como le dicen los mexicanos AWEBO... awebo wey!
-- Me ha costado... la neta que me ha costado
main :: IO ()
main = start gui

gui = do
    let dogs = [
            ("Boxer", "boxer.jpg"),
            ("Dogo", "dogo.jpg"), 
            ("Hot-dog", "hotdog.jpg")
            --En teoria, si agregas mas items, funciona!
            ]
        dogs_index = [0..length dogs - 1]
        dogs_names = [fst x | x <- dogs]
        dogs_paths = [snd x | x <- dogs]
    f <- frame [text := "Dog Choice"]
    window_image <- window f []
    radioBox_dogs <- radioBox f Vertical dogs_names [text := "Dogs"]
    set radioBox_dogs [on select := dogPaint dogs_paths window_image radioBox_dogs]
    imageList_dogs <- imageListFromFiles (sz 300 300) dogs_paths


    set f [layout := minsize (sz 500 600) $
        fill $ row 5 [
            vfill $ widget radioBox_dogs,
            fill $ widget window_image
            ]
        ]
    return ()
    where
        onPaint vbitmap dc viewArea = do
            mbBitmap <- get vbitmap value
            case mbBitmap of
                Nothing -> return ()
                Just bm -> drawBitmap dc bm pointZero False []
        dogPaint paths window' w = do
            i <- get w selection
            let fname = paths !! i
            putStrLn fname
            bm <- bitmapCreateFromFile fname
            vbitmap <- variable [value := Just bm]
            bmsize <- get bm size
            putStrLn $ show bmsize
            set window' [clientSize := bmsize, on paint := onPaint vbitmap]
            repaint window'
            return ()
