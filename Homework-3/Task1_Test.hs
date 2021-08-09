module Task1_Test where 

import Test.HUnit
import Task1

testTree =
  Node
    5
    ( Node
        22
        (Node 2 EmptyTree EmptyTree)
        (Node 6 EmptyTree EmptyTree)
    )
    ( Node
        1
        EmptyTree
        ( Node
            3
            (Node 111 EmptyTree EmptyTree)
            EmptyTree
        )
    )


testInorder = TestCase (assertEqual "Inorder works for example tree" (values Inorder testTree)  [2,22,6,5,1,111,3] )
testPostorder = TestCase (assertEqual "Postorder works for example tree" (values Postorder testTree)  [2,6,22,111,3,1,5] )
testPreorder = TestCase (assertEqual "Preorder works for example tree" (values Preorder testTree)  [5,22,2,6,1,3,111] )


task1Tests = TestList [testInorder, testPostorder, testPreorder]
main = runTestTT task1Tests