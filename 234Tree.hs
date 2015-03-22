----         Assignment 2
----Top-down 2-3-4 Trees in Haskell
 
----Killian Mills, 11368701
 
--3 types of nodes, two three and four
--implement an insertion function
--search function
--display function
 
--constrtuctors for the 2,3 and 4 trees
data Tree t = Empty
    | TwoTree t (Tree t)(Tree t)                                                                        -- first node type
    | ThreeTree t t (Tree t)(Tree t)(Tree t)                                            -- second node type
    | FourTree t t t (Tree t)(Tree t)(Tree t)(Tree t)                           -- third node type
        deriving (Eq, Ord)
-- two tree holds 1 element, has two children
-- three tree holds 2 elements, has three children
-- four tree holds 3 elements, has four children
 
--Test Tree
myTree = TwoTree 15 (FourTree 2 4 8 (TwoTree 1 Empty Empty) (TwoTree 3 Empty Empty) (TwoTree 6 Empty Empty)
        (TwoTree 9 Empty Empty)) (TwoTree 20 Empty (TwoTree 25 Empty Empty))
 
 
       
-------------- INSERT ---------------
insert :: (Ord t) => t -> Tree t -> Tree t
 
--bases
--1
insert inputVal Empty = TwoTree inputVal Empty Empty
 
--2
insert inputVal ( TwoTree firstVal Empty Empty)
        | inputVal < firstVal = (ThreeTree inputVal firstVal Empty Empty Empty)                                         -- input is first element
        | inputVal > firstVal = (ThreeTree firstVal inputVal Empty Empty Empty)                                         -- input is second element
       
--3
insert inputVal ( ThreeTree firstVal secondVal Empty Empty Empty)
        | inputVal < firstVal  = (FourTree inputVal firstVal secondVal Empty Empty Empty Empty)         -- input is first element
        | inputVal > secondVal = (FourTree firstVal secondVal inputVal Empty Empty Empty Empty)         -- input is second element
        | inputVal > firstVal  = (FourTree firstVal inputVal secondVal Empty Empty Empty Empty)         -- input is third element
 
--restructures 
--1
insert inputVal (TwoTree firstVal(FourTree left middle right Empty Empty Empty Empty ) (rightNode))                               -- 2 tree to 3 tree
        = insert inputVal (ThreeTree middle firstVal (TwoTree left Empty Empty) (TwoTree right Empty Empty) (rightNode)) -- 2 vals, 3 children
       
--2
insert inputVal (TwoTree firstVal (leftNode) (FourTree left middle right Empty Empty Empty Empty))                                -- 2 tree to 3 tree
        = insert inputVal (ThreeTree firstVal middle (leftNode) (TwoTree left Empty Empty) (TwoTree right Empty Empty )) -- 2 vals, 3 children
       
--3
insert inputVal (ThreeTree firstVal secondVal(FourTree left middle right Empty Empty Empty Empty) (middleNode) (rightNode))                      -- 3 tree to 4 tree
        = insert inputVal (FourTree middle firstVal secondVal(TwoTree left Empty Empty) (TwoTree right Empty Empty)(middleNode)(rightNode)) -- 3 vals, 4 children
       
--4
insert inputVal (ThreeTree firstVal secondVal (leftNode)(FourTree left middle right Empty Empty Empty Empty) (rightNode) )                       -- 3 tree to 4 tree
        = insert inputVal (FourTree middle firstVal secondVal (leftNode) (TwoTree left Empty Empty) (TwoTree right Empty Empty)(rightNode)) -- 3 vals, 4 children
 
--5
insert inputVal (ThreeTree firstVal secondVal (leftNode) (middleNode) (FourTree left middle right Empty Empty Empty Empty ))                      -- 3 tree to 4 tree
        = insert inputVal (FourTree firstVal secondVal middle (leftNode) (middleNode) (TwoTree left Empty Empty)(TwoTree right Empty Empty)) -- 3 vals, 4 children
       
--6
insert inputVal (FourTree firstVal secondVal thirdVal Empty Empty Empty Empty)                                                  -- 4 tree to 2 2 trees
        = insert inputVal (TwoTree secondVal (TwoTree firstVal Empty Empty ) (TwoTree thirdVal Empty Empty )) -- 1 val, 2 children
       
--inserts
--1
insert inputVal (TwoTree firstVal (leftNode) (rightNode))                                                                                                       -- two child nodes
        | inputVal < firstVal = (TwoTree firstVal ( insert inputVal leftNode) (rightNode))
        | inputVal > firstVal = (TwoTree firstVal (leftNode) ( insert inputVal rightNode))
       
--2
insert inputVal (ThreeTree firstVal secondVal (leftNode) (middleNode) (rightNode))                                                      -- three child nodes
        | inputVal < firstVal = ( ThreeTree firstVal secondVal (insert inputVal leftNode) middleNode rightNode)
        | inputVal > firstVal && inputVal < secondVal = ( ThreeTree firstVal secondVal (leftNode) (insert inputVal middleNode) (rightNode))
        | inputVal < firstVal && inputVal < secondVal = ( ThreeTree firstVal secondVal (leftNode) (middleNode) (insert inputVal rightNode))
       
--3
insert inputVal (FourTree firstVal secondVal thirdVal (leftNode) (middle1Node) (middle2Node) (rightNode))       -- four child nodes
        | inputVal < firstVal =  (FourTree firstVal secondVal thirdVal (insert inputVal leftNode) (middle1Node) (middle2Node) (rightNode))
        | inputVal > thirdVal =  (FourTree firstVal secondVal thirdVal (leftNode) (middle1Node) (middle2Node) (insert inputVal rightNode))
        | inputVal < secondVal = (FourTree firstVal secondVal thirdVal (leftNode) (insert inputVal middle1Node) (middle2Node) (rightNode))
        | inputVal > secondVal = (FourTree firstVal secondVal thirdVal (leftNode) (middle1Node) (insert inputVal middle2Node) (rightNode))
 
       
       
-------------- SEARCH ---------------
 
search :: (Ord t) => Tree t -> t -> Bool
 
--empty tree
search Empty inputVal = False
 
--2 tree
search (TwoTree firstVal (leftNode) (rightNode)) inputVal                                                                                       -- two child nodes
        | inputVal == firstVal = True   -- found input
        | inputVal < firstVal = search leftNode inputVal
        | inputVal > firstVal = search rightNode inputVal
 
--3 tree
search (ThreeTree firstVal secondVal (leftNode) (middleNode) (rightNode)) inputVal                                              -- three child nodes
        | inputVal == firstVal || inputVal == secondVal = True  -- found input
        | inputVal < firstVal  = search leftNode inputVal
        | inputVal > secondVal = search rightNode inputVal
        | inputVal > firstVal && inputVal < secondVal = search middleNode inputVal
       
--4 tree
search (FourTree firstVal secondVal thirdVal (leftNode) (middle1Node) (middle2Node) (rightNode)) inputVal       -- four child nodes
        | inputVal == firstVal || inputVal == secondVal || inputVal == thirdVal = True  -- found input
        | inputVal < firstVal = search leftNode inputVal
        | inputVal > thirdVal = search rightNode inputVal
        | inputVal > firstVal && inputVal < secondVal = search middle1Node inputVal
        | inputVal > secondVal && inputVal < thirdVal  = search middle2Node inputVal           
 
       
       
-------------- DISPLAY ---------------
 
--display
 
instance Show a => Show (Tree a) where
   show t = displayFormat t
 
indent :: [String] -> [String]
indent = map ("          "++)
 
--Empty case
displayTree Empty = []
 
--Two Tree case
displayTree (TwoTree firstVal (leftNode) (rightNode))
        = indent (displayTree rightNode) ++ ["[ " ++ show firstVal ++ " ]"] ++ indent (displayTree leftNode)
 
--Three Tree case      
displayTree (ThreeTree firstVal secondVal (leftNode) (middleNode) (rightNode))
        = indent (displayTree rightNode ) ++ ["[ " ++ show firstVal ++ " " ++ show secondVal ++ " ]"] ++ indent (displayTree middleNode) ++ indent (displayTree leftNode)
 
--Four Tree case
displayTree (FourTree firstVal secondVal thirdVal (leftNode) (middle1Node) (middle2Node) (rightNode))
        = indent (displayTree rightNode) ++ indent (displayTree middle2Node) ++ ["[ " ++ show firstVal ++ " " ++ show secondVal ++ " " ++ show thirdVal ++ " ]"] ++ indent (displayTree middle1Node) ++ indent (displayTree leftNode)
 
displayFormat :: (Show t) => Tree t -> String
displayFormat = unlines.displayTree
 
display :: (Show t) => Tree t -> IO ()
display t = putStr (displayFormat t)
       
       
       
---- END ----