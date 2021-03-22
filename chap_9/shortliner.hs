{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main = do
    contents <- getContents
    putStr (shortLineOnly contents)

shortLineOnly :: String -> String
shortLineOnly = unlines . filter (\line -> length line < 10) . lines
