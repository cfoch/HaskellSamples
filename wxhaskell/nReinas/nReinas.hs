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

