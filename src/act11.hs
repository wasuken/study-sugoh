quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
  where lt = filter (< x) xs
        gt = filter (>= x) xs
