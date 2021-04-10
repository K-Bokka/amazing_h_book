{-# OPTIONS -Wall -Werror #-}

lightCompare :: String -> String -> Ordering
lightCompare x y = let a = length x `compare` length y
                       b = x `compare` y
                   in if a == EQ then b else a

lightCompare' :: String -> String -> Ordering
lightCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

lightCompare'' :: String -> String -> Ordering
lightCompare'' x y = (length x `compare` length y) `mappend`
                     (vowels x `compare` vowels y) `mappend`
                     (x `compare` y)
                 where vowels = length . filter (`elem` "aiueo")
