-- CA320 Computability and Complexity
--
-- Killian Mills
--
--:load "H:/Complexity/exercise1.hs"
-- how to load in the command line of haskell
--
-- week 8 lab exam
-- about 4/5 questions raising in difficulty
--
-- week 7 assignment
 
cube :: Int -> Int -- cube takes in int, gives an int
cube x = x * x * x -- cuber x = x by x by x
 
edge, volume :: Int -- both are ints
edge = 3 -- edge = 3, set to 3
 
volume = cube edge -- volume = the cube of edge, cube of 3
 
surfaceArea :: Float -> Float -- surfaceArea takes in float, gives float
surfaceArea r = 4.0 * pi * r^2 -- radius = 4.0 by pi by radius raised to 2