{-# OPTIONS -Wall -Werror #-}

module Tree where

(-:) :: a -> (a -> b) -> b
x -: f = f x

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

changeToP' :: Tree Char -> Tree Char
changeToP' (Node x l (Node y (Node _ m n) r)) = (Node x l (Node y (Node 'P' m n) r))
changeToP' _ = error "Oops!"

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
