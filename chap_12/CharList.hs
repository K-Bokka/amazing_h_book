{-# OPTIONS -Wall -Werror #-}

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
