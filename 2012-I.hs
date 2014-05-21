cantidadFactores n 1 = 1
cantidadFactores n d
    | (r == 0) = 1 + cantidadFactores (n `div` d) d
    | (r /= 0) = 0
    where r = n `mod` d

primo 1 = False
primo n = foldl (\acc x -> if n `mod` x == 0 then False else acc) True [2.. (n - 1)]

factores n =  [x | x <- [(a, let b = cantidadFactores n a in b) | a <- [2..n]], primo (fst x), snd x /= 0]

type CoordenadaX = Float
type CoordenadaY = Float
type Vertice = (CoordenadaX, CoordenadaY)
data Poligono = Poli [Vertice] deriving (Show)

distancia :: Vertice -> Vertice -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


areaTriangulo :: Poligono -> Float
areaTriangulo (Poli [p1, p2, p3]) =
    sqrt (s * (s - a) * (s - b) * (s - c))
    where 
        a = distancia p1 p2
        b = distancia p2 p3
        c = distancia p1 p3
        s = (a + b + c)/2

area (Poli [v1,v2]) = 0
area (Poli (v1:v2:v3:vs)) = (areaTriangulo triangulo) + area (Poli  (v2:v3:vs))
    where triangulo = Poli [v1, v2, v3]

-- Ejemplo: area (Poli[(0.0, 0.0), (5.0, 0.0), (5.0, 2.0), (0.0, 2.0)])

ladosAux (Poli [v]) = []
ladosAux (Poli (v1:v2:vs)) = (distancia v1 v2):(ladosAux (Poli (v2:vs)))

ladosLista (Poli (v:vs)) = ladosAux (Poli (v:vs)) ++ [distancia v (last vs)]

esRegular :: Poligono -> Bool
esRegular (Poli p) = all (== head lados) lados
	where lados = ladosLista (Poli p)


type Valores = [Float]
type Monedas = [Integer]

val :: Valores
val = [ 0.01, 0.05, 0.10, 0.20, 0.50, 1.0, 2.0]

tiposDeMonedas = length val

c :: Monedas
c = [1, 0, 0, 0, 1, 2, 0]


valor :: Valores -> Monedas -> Float
valor v m = sum (zipWith (*) v (map fromIntegral m))

data Polinomio = Poli_nulo | Sumar_mono Int Int Polinomio deriving (Show)

sumar Poli_nulo q = q
sumar (Sumar_mono c i p) q = Sumar_mono c i (sumar p q)
