import Data.List

mapa = [
    ("Oradea", "Zerind", 71),
    ("Zerind", "Arad", 75),
    ("Arad", "Sibiu", 140),
    ("Oradea", "Sibiu", 151)
    ]

mapa2 = [
    ("Elio", "Cipreses", 20),
    ("Elio", "San Miguel", 90),
    ("Cipreses", "San Miguel", 50)
    ]

mapa_ mapa = mapa ++ [ (y, x, n) | (x, y, n) <- mapa]

ruta :: [Char] -> [Char] -> [([Char], [Char], Integer)] -> [([[Char]], Integer)]
ruta x y [] = []
ruta x y ((r, s, n):rs)
    | x == y = [([x], 0)]
    | (r == x) && (s == y) = [([x, y], n)]
    | (r /= x) && (s /= y) = ruta x y rs
--    | (r == x) && (s /= y) = [([x] ++ fst aux, n + snd aux)]
    | (r == x) && (s /= y) = if (rs == []) then [] else [([x] ++ fst aux, n + snd aux)]
        where
        aux = head $ ruta s y rs

--ruta x y ((r, s, n):(r',s', n'):rs) = 

{-
import Data.List
-}
{-iCiudad ciudad rutas = elemIndices ciudad [
    if (c1 == ciudad) || (c2 == ciudad) then ciudad
    else "Nada"
    | (c1, c2, n) <- rutas
    ]
-}

{-
ruta ciudad ciudad rutas =
    [([ciudad], 0)]
ruta ciudad1 ciudad2 rutas = 
-}
