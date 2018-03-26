-- pascals triangle
-- call pascalTriangle level
pascal :: [[Int]]
pascal = [[1]] ++ [x | y<-pascal, let x = [1] ++ myadd y ++ [1]]

myadd :: [Int] -> [Int]
myadd (x:[]) = []
myadd (x:y:xs) = [x+y] ++ myadd (y:xs)

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n pascal
