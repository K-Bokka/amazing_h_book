{-# OPTIONS -Wall -Werror #-}

import Data.List hiding (sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
