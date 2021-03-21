{-# OPTIONS -Wall -Werror #-}

infixr 5 :-:
data List a = Empty |  a :-: (List a) deriving (Show, Read, Eq, Ord)
