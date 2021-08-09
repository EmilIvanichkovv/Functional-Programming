makeLeftSubtree counter flag (x:xs)
    | flag == True && counter == 0 = ""
    | x == '('                     = x : ( makeLeftSubtree  (counter + 1) True xs )
    | x == '*' && counter == 0     = "*"
    | flag == False                = makeLeftSubtree counter flag xs
    | x == ')'                     = x : ( makeLeftSubtree (counter - 1) flag xs )
    | otherwise                    = x : ( makeLeftSubtree counter flag xs )

makeRightSubtree:: Int -> Bool -> String -> String
makeRightSubtree counter flag (x:xs)
    | flag == True && counter == 0 = makeLeftSubtree 0 False (x:xs)
    | x == '('                     = makeRightSubtree  (counter + 1) True xs 
    | flag == False                = makeRightSubtree counter flag xs
    | x == ')'                     = makeRightSubtree (counter - 1) flag xs
    | x == '*' && counter == 1    = "*"
    | otherwise                    = makeRightSubtree counter flag xs 