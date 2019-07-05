solveRPN :: String -> Double
solveRPN  = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y * x):ys
        foldingFunction (x:y:ys) "+" = (y + x):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:ys) "ln" = log x:ys
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs

data Section = Section { getA :: Int, getB :: Int, getC :: Int }
  deriving (Show)
type RoadSystem = [Section]

heathrowToLonadon = [ Section 50 10 10
                    , Section 5 90 20
                    , Section 40 2 25
                    , Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- optimalPath :: RoadSystem -> Path

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      forwardTimeToA = timeA + a
      crossTimeToA = timeB + b + c
      forwardTimeToB = timeB + b
      crossTimeToB = timeA + a + b
      newPathToA = if forwardTimeToA <= crossTimeToA
                   then (A, a):pathA
                   else (C, c):(B, b):pathB
      newPathToB = if forwardTimeToB <= crossTimeToB
                   then (B, b):pathB
                   else (C, c):(A, a):pathA
  in (newPathToA, newPathToB)
