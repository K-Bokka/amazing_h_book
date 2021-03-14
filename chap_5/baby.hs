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
