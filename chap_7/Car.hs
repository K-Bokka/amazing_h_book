{-# OPTIONS -Wall -Werror #-}

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving Show

data Car' a b c = Car' { company' :: a
                       , model' :: b
                       , year' :: c
                       } deriving Show
