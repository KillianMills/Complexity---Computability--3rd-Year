myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend (x:xs) y = x : myAppend xs y
 
myHead :: [a] -> a
myHead [] = error "Idiot alert, empty list"
myHead (x:_) = x
 
myLast :: [a] -> a
myLast [] = error "Idiot alert, empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
 
myInit :: [a] -> [a]
myInit [] = error "Idiot alert, empty list"
myInit [x] = []
myInit (x:y:[]) = [x]
myInit (x:xs) = x : myInit(xs)
 
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
 
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
 
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs
 
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum(xs)
 
myProduct :: Num a => [a] -> a
myProduct [] = error "Idiot alert, empty list"
myProduct [x] = x
myProduct (x:xs) = x * myProduct(xs)
 
myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Idiot alert, empty list"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)
 
myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Idiot alert, empty list"
myMinimum [x] = x
myMinimum (x:xs) = min x (myMinimum xs)
 
myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys)
        | x == y = True
        | otherwise = myElem x ys
 
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (x:xs)
        | a == x = xs
        | otherwise = x  : (myDelete a xs)
 
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect _ [] = []
myIntersect [] _ = []
myIntersect (x:xs) ys
        | myElem x ys = x : myIntersect xs ys
        | otherwise = myIntersect xs ys
       
       
       
       
--myAppend :: [a] -> [a] -> [a]
--myAppend [] [] = error "Idiot alert, empty lists"
--myAppend a b = a ++ b
                               
                               
--myHead :: [a] -> a
--myHead [] = error "Idiot alert, empty list"
--myHead (a:_) = a
 
--myLast ::(Eq a) => [a] -> a
--myLast [] = error "Idiot alert, empty list"
--myLast (x:xs) = if xs ==[]
        --                      then x
        --                      else myLast(xs)
                               
--myTail ::(Eq a) => [a] -> a
--myTail [] = error "Idiot alert, empty list"
--myTail (x:xs) = if xs ==[]
        --                      then x
        --                      else xs