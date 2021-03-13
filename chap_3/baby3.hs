{-# OPTIONS -Wall -Werror #-}
lucky :: Int -> String
lucky 7 = "Lucky Number Seven!"
lucky _ = "Sorry, you're out of luck"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5. Input Number is " ++ show x

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Empty String!"
firstLetter st@(x:_) = "The first letter of " ++ st ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight."
    | bmi <= 25.0 = "You're supposedly normal."
    | bmi <= 30.0 = "You're fat"
    | otherwise = "You're a whale"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / (height * height) <= 18.5 = "You're underweight."
    | weight / (height * height) <= 25.0 = "You're supposedly normal."
    | weight / (height * height) <= 30.0 = "You're fat"
    | otherwise = "You're a whale"

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight."
    | bmi <= normal = "You're supposedly normal."
    | bmi <= fat = "You're fat"
    | otherwise = "You're a whale"
    where bmi = weight / (height * height)
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

badGreeting :: String
badGreeting = "Oh! Pfft. "

niceGreeting :: String
niceGreeting = "Hello! "

greet :: String -> String
greet st@"Juan" = niceGreeting ++ st
greet st@"Fernando" = niceGreeting ++ st
greet name = badGreeting ++ name

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / (height * height)
