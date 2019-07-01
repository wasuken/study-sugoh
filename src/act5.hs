zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'  _ [] _ = []
zipWith'  _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

collatzNumberSeq :: Int -> [Int]
collatzNumberSeq x
  | x == 1 = [1]
  | even x = x:collatzNumberSeq (x `div` 2)
  | odd x = x: (collatzNumberSeq (x * 3 + 1))
