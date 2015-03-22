--QUESTION 1--
nums7 :: [Int]
nums7 = [n | n <-[7,14..200],mod n 2 ==0]
 
--QUESTION 2--
listNums :: Int -> Int -> [Int]
listNums a b = [a..b]
       
--QUESTION 3-- 
diff :: (Eq a) => [a] -> [a] -> [a] -- all of type a, any type
diff a b = [a | a <- a, not (elem a b)]
 
--QUESTION 4--
data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                                deriving (Eq, Ord, Show)
               
--QUESTION 5--
preorder :: BinTree a -> [a]
preorder Empty = []
preorder (Root x l r) = preorder l ++ preorder r ++ [x]
 
 
--EXTRAS--
union :: (Eq a) => [a] -> [a] -> [a]
union a b = [ a | a <- a, not ( elem a b)] ++ [a | a <-b]
 
intersection ::(Eq a)=> [a] -> [a] -> [a]
intersection a b = [ a | a <- a, (elem a b )