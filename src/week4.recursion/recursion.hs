maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum2 :: (Ord a) => [a] -> a  
maximum2 [] = error "maximum of empty list"  
maximum2 [x] = x  
maximum2 (x:xs) = max x (maximum' xs)  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

reverse2 :: [a] -> [a]
reverse2 x = nested x []
    where nested :: [a] -> [a] -> [a]
          -- netsed [] t = t
          -- nested (x:xs) t = nested xs (x:t)
          nested list acc = case list of [] -> acc
                                         (y:ys) -> nested ys (acc:y)


fibonacci :: (Num a, Ord a) => a -> a
fibonacci a
    | a <= 1 = 1
    | otherwise = fibonacci(a - 1) + fibonacci(a - 2)

getOneToThree :: (Num a, Show a) => a -> String
getOneToThree x = "N is " ++ str x
    where str y = show y
