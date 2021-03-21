{-# OPTIONS -Wall -Werror #-}

class YesNo a where
    yn :: a -> Bool
