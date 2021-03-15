{-# OPTIONS -Wall -Werror #-}

import Data.List (nub)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
