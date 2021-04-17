{-# OPTIONS -Wall -Werror #-}

import Data.Monoid

isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compare gang size to 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, preLog) f = let (y, newLog) = f x in (y, preLog ++ newLog)

applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, preLog) f = let (y, newLog) = f x in (y, preLog `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "herky" = ("whiskey", Sum 99)
addDrink _ = ("Beer", Sum 30)
