{-# OPTIONS -Wall -Werror #-}

isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compare gang size to 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, preLog) f = let (y, newLog) = f x in (y, preLog ++ newLog)
