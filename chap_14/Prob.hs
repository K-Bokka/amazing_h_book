{-# OPTIONS -Wall -Werror #-}

import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    (Prob fs) <*> (Prob xs) = Prob [(f x, r) | (f, _) <- fs, (x, r) <- xs]

instance Monad Prob where
    m >>= f = flatten (fmap f m)

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a', 1%2), ('b', 1%2)], 1%4)
    ,(Prob [('c', 1%2), ('d', 1%2)], 3%4)
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multiAll xs
    where multiAll (Prob ixs, p) = map (\(x, r) -> (x, p*r)) ixs
