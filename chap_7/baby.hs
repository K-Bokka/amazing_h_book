{-# OPTIONS -Wall -Werror #-}

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

area :: Shape -> Float
area (Circle _ r) = pi * r ^ (2 :: Integer)
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 -y1)
