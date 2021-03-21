{-# OPTIONS -Wall -Werror #-}

import Tree
import TrafficLight

class YesNo a where
    yn :: a -> Bool

instance YesNo Int where
    yn 0 = False
    yn _ = True

instance YesNo [a] where
    yn [] = False
    yn _ = True

instance YesNo Bool where
    yn = id

instance YesNo (Maybe a) where
    yn (Just _) = True
    yn Nothing = False

instance YesNo (Tree a) where
    yn EmptyTree = False
    yn _ = True

instance YesNo TrafficLight where
    yn Red = False
    yn _ = True

ynIf :: (YesNo y) => y -> a -> a -> a
ynIf val yes no = if yn val then yes else no
