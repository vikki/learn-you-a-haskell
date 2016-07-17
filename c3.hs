compress' :: (Ord a) => [a] -> [a]
compress' [] = []
compress' [x] = [x] 
compress' (x:v:t)
    | x == v = [x] ++ compress' (t)
    | otherwise = [x] ++ compress' (v:t)

pack' :: [String] -> [String]
pack' [] = []
pack' [x] = [x]
pack' (x:v:t) = x ++ v ++ (pack' (v:t))
   -- | x == v = (x ++ v) : pack' (t)
   -- | otherwise = x : pack' (v:t)


-- ["aa"] then you want to get to ["aaa"]
-- which is "a":["aa"]
