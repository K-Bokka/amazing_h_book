
import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
treeInsert _ _ = error "The second argument must be a Tree type."

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
treeElem _ _ = error "The second argument must be a Tree type."

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance F.Foldable Tree where
    foldMap _ EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x `mappend`
                             F.foldMap f r
