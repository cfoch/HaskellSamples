import Graphics.UI.WX

main = start go

go = frame [on paint := p]
p dc view = sierpinskiTri p 50 300 256

    where    
      minSize :: Int
      minSize = 8

      fillTri w x y size = polygon dc [(pt x y),(pt (x+size) y),( pt x (y-size))] []
    
      sierpinskiTri w x y size
                  = if size <= minSize
                  then fillTri w x y size
                  else let size2 = size `div` 2
                  in do sierpinskiTri w  x  y        size2
                        sierpinskiTri w  x (y-size2) size2
                        sierpinskiTri w (x+size2)  y size2


