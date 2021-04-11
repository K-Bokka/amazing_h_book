{-# OPTIONS -Wall -Werror #-}

import MonadPlus

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2, r-1)
                ,(c+2, r+1)
                ,(c-2, r-1)
                ,(c-2, r+1)
                ,(c+1, r-2)
                ,(c+1, r+2)
                ,(c-1, r-2)
                ,(c-1, r+2)
                ]
    guard (elem c' [1..8] && elem r' [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = elem end $ in3 start
