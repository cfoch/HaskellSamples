list2Int :: (Num a) => [a] -> a
list2Int [x] = x
list2Int all@(x:xs) = x * 10 ^ (length all - 1)+ list2Int xs

int2List :: (Integral a) => a -> [a]
int2List 0 = []
int2List x = int2List (x `div` 10) ++ [x `mod` 10]

nfibonacci :: (Num a) => Int -> [a]
nfibonacci n
    | n == 1 = [0]
    | n == 2 = [0, 1]
    | otherwise = l ++ [(l !! (n - 2)) + (l !! (n - 3))]
    where l = nfibonacci (n - 1)

fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

nfibonacci' :: Int -> [Int]
nfibonacci' n = [x | x <- map fibonacci [1..n]]


