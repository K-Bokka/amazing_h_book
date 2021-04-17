{-# OPTIONS -Wall -Werror #-}

module Stack where

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop [] = error "Empty Stack!!!"
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)
