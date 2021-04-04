{-# OPTIONS -Wall -Werror #-}

import System.IO

main :: IO ()
main = do
     handle <- openFile "baabaa.txt" ReadMode
     contents <- hGetContents handle
     putStr contents
     hClose handle
