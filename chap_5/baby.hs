{-# OPTIONS -Wall -Werror #-}

multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z

multiThree' :: Int -> (Int -> (Int -> Int))
multiThree' x y z = x * y * z

multiTwoWithNine :: Int -> (Int -> Int)
multiTwoWithNine = multiThree' 9

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphabet :: Char -> Bool
isUpperAlphabet = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        lager = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort lager

largestDivisible :: Integer
largestDivisible = head (filter p [100000,999999..])
    where p x = mod x 3829 == 0
