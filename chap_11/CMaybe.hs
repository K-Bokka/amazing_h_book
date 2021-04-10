{-# OPTIONS -Wall -Werror #-}

data CMaybe a = CNothing | CJust Int a deriving Show

--this is not Functor
instance Functor CMaybe where
    fmap _ CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
