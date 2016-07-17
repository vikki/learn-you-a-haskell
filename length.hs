length' :: (Num a) => [a] -> a
length' [] = error "bah"
length' (_:x)  = sum [ 1 | _ <- x ] 

length'' :: (Num b) => [a] -> b  
length'' [] = 0 
length'' (_:xs) = 1 + length'' xs

kloutTell :: (Integral a) => a -> String
kloutTell kloutScore
	| kloutScore < 20 = "Lammmmmme. Waste more of your life on the internets!!!"
        | kloutScore < 40 = "Not bad, but you need to post more x things about y lists, and photos of kittens."
        | otherwise       = "Awesome-sauce. You can use that fancy lounge in SFO - check you out!"
 
--kloutTell :: (Float a) => a -> String
--kloutTell x =  (kloutTell.round) x

kitty :: (Ord a,Num a) => a -> a -> String
kitty a b 
    | c < 10 = "foo"
    | c < 20 = "bar"
    | otherwise = "baz"
    where c = a*b


kitty' :: (Ord a,Num a) => a -> a -> String
kitty' a b
    | c < foo = "foo"
    | c < bar = "bar"
    | otherwise = "baz"
    where c = a*b
          foo = 10
          bar = 20

kitty'' :: (Ord a,Num a) => a -> a -> String
kitty'' a b
    | c < foo = "foo"
    | c < bar = "bar"
    | otherwise = "baz"
    where c = a*b
          (foo, bar) =  (10, 20)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: (Num a) => [a] -> [a] -> [(a,a)]
zip' [] _ = []
zip' _ [] = []
zip' [x,y] [c,b] = [(x,c), (y,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Ord a) => a-> [a] -> Bool
elem' _ [] = False
elem' a [x] = a == x
elem' a (x:xs) = a == x || elem' a xs 
