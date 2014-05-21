--Conjuntos
data Conjunto = Cjto_vacio | Anadir Int Conjunto deriving (Show)

--Ej:
--(Anadir 3 (Anadir 5 (Anadir 2 (Anadir 4 (Anadir 1 (Anadir 2 Cjto_vacio))))))

unit :: Int -> Conjunto
unit e = Anadir e Cjto_vacio

es_cjto_vacio :: Conjunto -> Bool
es_cjto_vacio Cjto_vacio = True
es_cjto_vacio (Anadir e x) = False

esta :: Int -> Conjunto -> Bool
esta e Cjto_vacio = False
esta e (Anadir f x) = (e == f) || (esta e x)


quitar e Cjto_vacio = Cjto_vacio
quitar e (Anadir f x)
    | e == f = quitar e x
    | e /= f = Anadir f (quitar e x)

union Cjto_vacio y = y
union (Anadir e x) y = Anadir e (union x y)

interseccion Cjto_vacio y = Cjto_vacio
interseccion (Anadir e x) y
    |not (esta e y) = interseccion x y
    |esta e y = Anadir e (interseccion x y)

diferencia x Cjto_vacio = x
diferencia x (Anadir e y) = diferencia (quitar e x) y


