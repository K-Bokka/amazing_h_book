{-# OPTIONS -Wall -Werror #-}

respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

main :: IO ()
main = interact respondPalindromes
