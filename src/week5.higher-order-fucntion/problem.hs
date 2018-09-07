reverse_string xs = foldr (\x acc -> (toEnum x :: Char) : acc) [] xs
a = [105,108,105,107,101,104,97,115,107,101,108,108]
