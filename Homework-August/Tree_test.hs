module Tree_test where 

import Test.HUnit
import Tree

testTree =  (Node "Бозайник ли е?" 
    (Node "Голямо ли е?" 
        (Node "слон" EmptyTree EmptyTree ) 
        (Node "котка" EmptyTree EmptyTree ) )
    (Node "Лети ли?" 
        (Node "пчела" EmptyTree EmptyTree ) 
        (Node "златна рибка" EmptyTree EmptyTree ) ))

newNode = (Node "Има ли рог?" (Node "носорог" EmptyTree EmptyTree ) (Node "слон" EmptyTree EmptyTree ) )

enrichedTree = (Node "Бозайник ли е?" 
    (Node "Голямо ли е?" 
        (Node "Има ли рог?" 
            (Node "носорог" EmptyTree EmptyTree ) 
            (Node "слон" EmptyTree EmptyTree ) )
        (Node "котка" EmptyTree EmptyTree ) )
    (Node "Лети ли?" 
        (Node "пчела" EmptyTree EmptyTree ) 
        (Node "златна рибка" EmptyTree EmptyTree ) ))

correctString = "(Бозайник ли е?(Голямо ли е?(слон**)(котка**))(Лети ли?(пчела**)(златна рибка**)))"
correctInputForLeftRightValue = "Бозайник ли е?(Голямо ли е?(слон**)(котка**))(Лети ли?(пчела**)(златна рибка**)))"
leftSubsting = "(Голямо ли е?(слон**)(котка**))"
rightSubstring = "(Лети ли?(пчела**)(златна рибка**))"

test_treeToString     = TestCase $ assertEqual "Function 'treeToString' Test " (treeToString testTree) correctString
test_makeLeftSubtree  = TestCase $ assertEqual "Function 'makeLeftSubtree' Test" (makeLeftSubtree 0 False correctInputForLeftRightValue) leftSubsting
test_makeRightSubtree = TestCase $ assertEqual "Function 'makeRightSubtree' Test" (makeRightSubtree 0 False correctInputForLeftRightValue) rightSubstring
test_parseTree        = TestCase $ assertEqual "Function 'parseTree' Test" (parseTree correctString) testTree
test_takeValue        = TestCase $ assertEqual "Function 'takeValue' Test" (takeValue correctInputForLeftRightValue) (value testTree)
test_treeWithNewInfo  = TestCase $ assertEqual "Function 'treeWithNewInfo' Test" (treeWithNewInfo "Има ли рог?" "носорог" (Node "слон" EmptyTree EmptyTree)) newNode
test_treeEnrichment   = TestCase $ assertEqual "Function 'treeEnrichment' Test" (treeEnrichment "слон" testTree newNode) enrichedTree


tests = TestList  [test_treeToString, test_makeLeftSubtree, test_makeRightSubtree, test_parseTree, test_takeValue, test_treeWithNewInfo, test_treeEnrichment]
main = runTestTT tests