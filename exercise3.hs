type Poly = [Integer]
 
sumPoly :: Poly -> Poly -> Poly
sumPoly xs [] = xs
sumPoly [] ys = ys
sumPoly (x:xs) (y:ys) = (x+y) : sumPoly xs ys
 
evalPoly :: Int -> [Int] -> Int
evalPoly x [] = 0
evalPoly a (x:xs) = x + a * (evalPoly a xs)
 
 
 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)
 
 
 
shortest :: [[a]] -> [a]
shortest [] = []
shortest [soleList] = soleList
shortest (firstList : remainingLists) =
    shortestOfTwo firstList (shortest remainingLists)
 
shortestOfTwo firstList secondList =
    if length firstList < length secondList
      then firstList
      else secondList