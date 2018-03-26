--Standard coinchange problem using DP
--call coinchange value [coins]

func :: [Int] -> Int -> [Int]
func xs j = take j xs ++ [xs!!j + 1] ++ drop j xs

solve:: Int -> [Int] -> [[Int]] -> [[Int]]
solve n coins y1 = y1 ++ [get_best [(head x + 1) : func (tail x) j | j<-[0..(length coins - 1)], let y = coins!!j, let x = y1!!(n-y), n>=y] ]

get_best :: [[Int]] -> [Int]
get_best [] =  error "no coins encountered"
get_best (xs:[]) = xs
get_best (xs:xss) = get_best1 xs $ get_best xss

get_best1 :: [Int] -> [Int] -> [Int]
get_best1 x y| (x!!0 < y!!0) = x
             | (x!!0 > y!!0) = y
             | (tail x > tail y) = x
             | otherwise = y

memotization :: Int -> [Int] -> [[Int]]
memotization n coins | (n==0) = [take (1+(length coins)) (repeat 0)]
                     | otherwise = solve n coins (memotization (n-1) coins)

get_count :: [Int] -> [Int]
get_count y = y

coinChange :: Int -> [Int] -> [Int]
coinChange n coins =  tail (last (memotization n coins))
