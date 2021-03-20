{-# OPTIONS -Wall -Werror #-}

data Vector a = Vector a a a deriving Show

vPlus :: (Num a) => Vector a -> Vector a -> Vector a
vPlus (Vector i j k ) (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k ) (Vector l m n) = i * l + j * m + k * n

vMult :: (Num a) => Vector a -> a -> Vector a
vMult (Vector i j k ) m = Vector (i * m) (j * m) (k * m)

