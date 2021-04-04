{-# OPTIONS -Wall -Werror #-}

import Data.Char

main :: IO ()
main = do
    contents <- readFile "baabaa.txt"
    writeFile "baabaacaps.txt" (map toUpper contents)
