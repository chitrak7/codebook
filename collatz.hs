--collatz without DP
--call collatz

{-# LANGUAGE TemplateHaskell #-}

lenchain :: Int -> Int
lenchain n| n==1 = 1
          | even n = lenchain (n`div`2) + 1
          | otherwise = lenchain (1 + 3*n)  + 1

collatz :: Int -> Int
collatz n = arrayMax (map lenchain [1..n])

arrayMax:: (Ord a) => [a] -> a
arrayMax (x:[]) = x
arrayMax (x:xs) = max x (arrayMax xs)
