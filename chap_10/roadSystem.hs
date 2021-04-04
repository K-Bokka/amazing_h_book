{-# OPTIONS -Wall -Werror #-}

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving Show

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

data Label = A | B | C deriving Show
type Path = [(Label, Int)]

--optimalPath :: RoadSystem -> Path

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum $ map snd pathA
        timeB = sum $ map snd pathB
        forwardA = timeA + a
        forwardB = timeB + b
        crossA = forwardB + c
        crossB = forwardA + c
        newPathA = if forwardA <= crossA
                   then (A, a):pathA
                   else (C, c):(B, b):pathB
        newPathB = if forwardB <= crossB
                   then (B, b):pathB
                   else (C, c):(A, a):pathA
    in (newPathA, newPathB)
