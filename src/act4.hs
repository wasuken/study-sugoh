quicksort :: Ord a  => [a] -> [a]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort (x:xs) = (quicksort left) ++ (x:(quicksort right))
  where left = filter (<= x) xs
        right = filter (> x) xs
