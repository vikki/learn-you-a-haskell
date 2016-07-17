foo :: (Num a) => a -> a -> a
foo x y = x * y

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

-- try to do this to return which ones are long chains, not just the length
-- think you'd be better off using a list comprehension to do that? but not sure

numLongChains :: Int
numLongChains = length(filter (>15) (map (length.chain) [1..100]))

numLongChains' :: Int  
numLongChains' = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

-- define a lamba that says for an element xs
-- return true iff the length of xs is more than 15
-- do this to the result of calling chain on every element from 1-100
-- filter uses the lambda to remove stuff that doesn't match our lambda predicate
-- and then we finally return the length of this array
numLongChains'' :: Int  
numLongChains'' = length (filter (\xs -> length xs > 15) (map chain [1..100]))  
