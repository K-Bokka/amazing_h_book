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
