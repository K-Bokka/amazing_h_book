{-# OPTIONS -Wall -Werror #-}

-- this is not Cool
data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"
