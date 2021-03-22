{-# OPTIONS -Wall -Werror #-}

import Data.Char

main :: IO ()
main = do
    c <- getContents
    putStrLn $ map toUpper c
