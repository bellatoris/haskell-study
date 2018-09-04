lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The LIst has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Thie list is long. The first two elements are: " ++ show x ++ " and " ++ show y

printList :: (Show a) => [a] -> String
printList [] = "The list is empty"
printList (x:[]) = show x
printList (x:xs) = show x ++ " " ++ printList xs

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2 

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

{- The difference is that let bindings are expressions themselves. where bindings are just syntactic constructs. -}

a = (let b = 3 in b + 4)

head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

printList2 :: (Show a) => [a] -> String
printList2 xs = case xs of [] -> ""
                           (x:[]) -> show x
                           (x:xs2) -> show x ++ " " ++ printList2 xs2

describeList2 :: [a] -> String  
describeList2 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  

printList3 :: (Show a) => [a] -> String
printList3 l = let p [] = ""
                   p (x:[]) = show x
                   p (x:xs) = show x ++ " " ++ p xs
               in p l
