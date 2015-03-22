diff :: Int -> Int -> Int
diff x y = abs (x-y)
 
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
                         let s =(a + b + c) / 2
                         in sqrt(s*(s-a)*(s-b)*(s-c))
                         
isSum :: Int -> Int -> Int -> Bool
isSum a b c =
                        if ( a + b == c || a + c == b ||  b + c == a )
                                then True
                                        else False
                                       
ex3 :: Float -> Float -> Float -> Float
ex3 a b c =
                if ( a + b < c || a + c < b ||  b + c < a )
                        then error "Idiot alert, not a triangle"
                                else
                                        let s =(a + b + c) / 2
                                        in sqrt(s*(s-a)*(s-b)*(s-c))