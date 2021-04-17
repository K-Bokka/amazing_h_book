{-# OPTIONS -Wall -Werror #-}

module Stack where

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop [] = error "Empty Stack!!!"
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackMan :: Stack -> (Int, Stack)
stackMan stack = let
    ((), newStack1) = push 3 stack
    (_, newStack2) = pop newStack1
    in pop newStack2
