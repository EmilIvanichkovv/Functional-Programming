module Task1 where

data Tree a
  = EmptyTree
  | Node
      { value :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show, Read)

getValue :: Tree a -> a
getValue (Node value left right) = value

getLeft :: Tree a -> Tree a
getLeft (Node value left right) = left

getRight :: Tree a -> Tree a
getRight (Node value left right) = right

data Strategy = Inorder | Postorder | Preorder deriving (Show, Read)

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder tree = (inorder (getLeft tree)) ++ [getValue tree] ++ (inorder (getRight tree))

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder tree = [getValue tree] ++ (preorder (getLeft tree)) ++ (preorder (getRight tree))

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder tree = (postorder (getLeft tree)) ++ (postorder (getRight tree)) ++ [getValue tree]

instance Eq Strategy where
  Inorder == Inorder = True
  Postorder == Postorder = True
  Preorder == Preorder = True
  _ == _ = False

values :: Strategy -> (Tree a) -> [a]
values s t
  | s == Inorder = inorder t
  | s == Postorder = postorder t
  | s == Preorder = preorder t
  | otherwise = []

