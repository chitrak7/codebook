
--powerset without using foldr
--call poerset
powerset :: (Eq a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [([x] ++ z) | z<- powerset xs]

--powerset with foldr
helper :: (Eq a) => a -> [[a]] -> [[a]] 
helper x ys = ys ++ [([x]++z) | z<-ys]

powersetWithFoldr :: (Eq a) => [a] -> [[a]]
powersetWithFoldr x = foldr helper [[]] x
