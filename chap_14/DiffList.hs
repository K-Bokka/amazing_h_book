{-# OPTIONS -Wall -Werror #-}

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (<>) (DiffList f) (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
