countdown :: (Num a) => [a] -> a
countdown xs = last xs

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

foo :: (Num a, Eq a) => [a] -> [a]
foo xs = [x | x <- init xs, x*2 == last xs]

-- this does like a tenth of the actual puzzle!
bar :: (Num a, Eq a) => [a] -> [a]
bar xs = [x | x <- init xs, (sum.init) xs == last xs]
