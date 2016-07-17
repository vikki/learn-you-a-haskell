-- ideas for 16 drop every nth el
-- keep a counter (or use elemIndex - see hoogle) to figure out the index we're currently on
-- then check if that index % n == 0, and if so don't include it in the result (otherwise do)

length' :: (Num a, Eq a, Num b) => a -> [a] -> b  
length' _ [] = 0  
length' a (x:xs)
  | x == a = 1 + length' a xs  
  | otherwise = length' a xs

idx' :: (Num a, Eq a, Num b) => a -> [a] -> b  
idx' _ [] = 0  
idx' a (x:xs)
  | x == a = 1 + length' a xs  
  | otherwise = length' a xs

foo' :: (Num a) => Int -> [a] -> [a]
foo' _ [] = []
foo' 1 [a] = []
foo' _ [a] = [a]
foo' n (x:xs)
   | length xs `mod` n == 0 = x: (foo' n (tail xs))
   | otherwise = foo' n xs  

-- 17 split a function at specified nth param
-- takewhile with a predicate for idx less than n, joined with the rest of the list

split' :: [a] -> Int -> ([a], [a])
split' (x:xs) a = ([x], xs)


-- extract a slice of an array - sth like [4:6] so you define the start and end bounds of the slice
