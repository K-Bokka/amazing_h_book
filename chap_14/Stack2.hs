{-# OPTIONS -Wall -Werror #-}

module Stack2 where

import Control.Monad.State

type Stack2 = [Int]

pop :: State Stack2 Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack2 ()
push a = state $ \xs -> ((), a:xs)

stackMan :: State Stack2 Int
stackMan = do
    push 3
    _ <- pop
    pop
