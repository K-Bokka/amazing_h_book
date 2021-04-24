{-# OPTIONS -Wall -Werror #-}

import Crumb
import Tree

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
