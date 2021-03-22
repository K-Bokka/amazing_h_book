{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main = interact shortLineOnly

shortLineOnly :: String -> String
shortLineOnly = unlines . filter (\line -> length line < 10) . lines
