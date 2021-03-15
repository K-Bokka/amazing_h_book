{-# OPTIONS -Wall -Werror #-}

import Data.List
import qualified Data.Map.Strict as M
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

fruits::[(String,Int)]
fruits = [("apple",1),("orange",2),("banana",3),("peach",4),("cherry",5),("orange",6),("apple",7),("peach",8)]

fruitsMap :: M.Map String Int
fruitsMap = M.fromList fruits

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

phoneBook :: [(String, String)]
phoneBook =
    [("betty", "1234-5678")
    ,("bonny", "2234-5678")
    ,("pasty", "3234-5678")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, _) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs
