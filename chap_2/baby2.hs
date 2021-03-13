removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z'] ]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
