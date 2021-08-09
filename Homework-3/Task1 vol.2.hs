data Tree a = EmptyTree | Node{
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                            }deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)
testTree=Node 
            1
            (Node 2 (Node 4 (Node 7 EmptyTree EmptyTree) EmptyTree)(Node 5 EmptyTree EmptyTree))
            (Node 3 (Node 6 (Node 8 EmptyTree EmptyTree) (Node 9 EmptyTree EmptyTree)) EmptyTree)

instance Eq Strategy where
    Inorder   == Inorder  =True 
    Postorder == Postorder=True
    Preorder  == Preorder =True
    _ == _ = False  


values :: Strategy -> (Tree a) -> [a]
values s t
    | s==Inorder   = inorder t
    | s==Postorder = postorder t
    | s==Preorder  = preorder t
    | otherwise    = []
    where 
        inorder EmptyTree = []
        inorder (Node root l r) = inorder l ++ (root : inorder r)
        postorder EmptyTree = []
        postorder (Node root l r) = postorder l ++ postorder r ++ [root]
        preorder EmptyTree = []
        preorder (Node root l r) = root : preorder l ++ preorder r

---------------------------------------------------------------------------------
inorder EmptyTree =[]
inorder (Node root l r) = inorder l ++ [root] ++ inorder r

postorder EmptyTree =[]
postorder (Node root l r) = postorder l ++ postorder r ++ [root]

preorder EmptyTree = []
preorder (Node root l r) = root : preorder l ++ preorder r

values' :: Strategy -> Tree a -> [a]
values' Inorder t =  inorder t
values' Postorder t = postorder t
values' Preorder t = preorder t
