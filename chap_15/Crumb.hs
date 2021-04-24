{-# OPTIONS -Wall -Werror #-}
module Crumb where

import Tree

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeft (Empty, _) = error "It's a dead end!"

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)
goRight (Empty, _) = error "It's a dead end!"

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
goUp (Empty, _) = error "It's at the top of the tree!"
goUp (_, []) = error "Didn't drop the breadcrumbs!"
