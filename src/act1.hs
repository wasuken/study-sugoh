listInMax :: [Int] -> Int
listInMax [] = error "Error list is empty"
listInMax [x] = x
listInMax (x:xs) = max x (listInMax xs)
