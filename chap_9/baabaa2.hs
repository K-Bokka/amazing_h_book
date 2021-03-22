{-# OPTIONS -Wall -Werror #-}

import System.IO

main :: IO ()
main = do
    withFile "baabaa.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents
