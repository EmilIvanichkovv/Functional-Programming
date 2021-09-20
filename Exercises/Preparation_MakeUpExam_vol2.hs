module Preparation_MakeUpExam_vol2  where

--Task1 Variant1
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

sampleTree = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))
treeWords::Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node v EmptyTree EmptyTree) = [[v]] 
treeWords (Node v l r) = map (v:) (lefts ++ right)
    where 
        lefts = treeWords l
        right = treeWords r