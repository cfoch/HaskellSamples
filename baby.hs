doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
    if x > 100
        then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

{-
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-}

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

{-
largestDivisible :: (Integral a) => a
largestDivisible x = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0
-}

product' :: (Num a) => [a] -> a
product' = foldl1 (*) 
--product' = foldl (\acc x -> x * acc) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> (if p x then x:acc else acc)) []

head' :: [a] -> a
head' = foldl1 (\acc x -> acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

last' :: [a] -> a
last' = foldr1 (\x acc -> acc)
