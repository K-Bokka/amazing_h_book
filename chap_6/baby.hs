{-# OPTIONS -Wall -Werror #-}

import Data.List
import qualified Data.Map.Strict as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

fruits::[(String,Int)]
fruits = [("apple",1),("orange",2),("banana",3),("peach",4),("cherry",5),("orange",6),("apple",7),("peach",8)]

fruitsMap :: M.Map String Int
fruitsMap = M.fromList fruits

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
