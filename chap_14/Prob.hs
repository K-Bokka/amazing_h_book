{-# OPTIONS -Wall -Werror #-}

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show
