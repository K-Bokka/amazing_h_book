{-# OPTIONS -Wall -Werror #-}
import DiffList

import Control.Monad.Writer

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
          tell ["Finished with " ++ show a]
          return a
    | otherwise = do
          result <- gcdReverse b (a `mod` b)
          tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
          return result

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
          tell $ toDiffList $ ["Finished with " ++ show a]
          return a
    | otherwise = do
          result <- gcdReverse' b (a `mod` b)
          tell $ toDiffList $ [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
          return result
