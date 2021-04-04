{-# OPTIONS -Wall -Werror #-}

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving Show

type RoadSystem = [Section]

data Label = A | B | C deriving Show
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestA, bestB) = foldl roadStep ([], []) roadSystem
    in if (sum $ map snd bestA) <= (sum $ map snd bestB)
       then reverse bestA
       else reverse bestB

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

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main :: IO ()
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a, b, c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ (show pathTime)
