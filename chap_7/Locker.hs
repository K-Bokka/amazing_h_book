{-# OPTIONS -Wall -Werror #-}

import qualified Data.Map.Strict as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num m = case Map.lookup num m of
    Nothing -> Left $ "Locker " ++ show num ++ " dosen't exists!"
    Just (state, code) -> if state == Free
                          then Right code
                          else Left $ "Locker " ++ show num ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "ISQA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]
