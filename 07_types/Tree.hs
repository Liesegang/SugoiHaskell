module Tree
  ( Tree(..)
  , singleton
  , treeInsert
  , treeElem
  ) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show = showTree 0

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

rep :: String -> Int -> String
rep str num = concat . replicate num $ str

showTree :: (Show a) => Int -> Tree a -> String
showTree n EmptyTree = ("  " `rep` n) ++ "None"
showTree n (Node x left right) = ("  " `rep` n) ++ "(" ++ show x ++ "\n" ++ showTree (n+1) left ++ "\n" ++ showTree (n+1) right ++ "\n" ++ ("  " `rep` n) ++ ")"

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert a (Node x left right)
    | a == x = Node a left right
    | a < x  = Node x (treeInsert a left) right
    | a > x  = Node x left (treeInsert a right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right
