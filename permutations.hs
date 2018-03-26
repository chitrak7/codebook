-- perms
-- A haskell code to compute al permutations of a list.
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = let y = perms xs
               in [a| i<-[0..length xs], k<- y, let a = take i k ++ [x] ++ drop i k ]
                 
