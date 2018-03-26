--split
--A function to split an array at all junctions

split :: [a] -> [([a],[a])]
split xs = [x| z<-[0..length xs], let x = (take z xs, drop z xs)]
