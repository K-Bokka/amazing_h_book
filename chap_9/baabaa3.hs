{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main = do
    contents <- readFile "baabaa.txt"
    putStr contents
