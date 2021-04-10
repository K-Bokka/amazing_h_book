{-# OPTIONS -Wall -Werror #-}

lightCompare :: String -> String -> Ordering
lightCompare x y = let a = length x `compare` length y
                       b = x `compare` y
                   in if a == EQ then b else a
