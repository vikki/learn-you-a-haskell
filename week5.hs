reverse' :: [a] ->  [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs  

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [_] = True
isPalindrome [] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome (init xs))


-- fuck yes.
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = foldl compareFlipped True flipReverseIt
  where compareFlipped = (\acc x -> acc && ((fst x) == (snd x)) ) 
        flipReverseIt = zip xs (reverse xs)


data NestedList a =  Elem a | List [NestedList a] 

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
flatten (List []) = []





-- argh everything looks like recursion :'(

count' :: (Real b) => [a] -> b 
count' xs = foldl(\acc _ -> acc + 1) 0 xs

sum' :: (Real a) => [a] -> a 
sum' xs = foldl(\acc x -> acc + x) 0 xs

product' :: (Real a) => [a] -> a 
product' [] = 0
product' xs = foldl(\acc x -> acc * x) 1 xs

average' :: (Real a, Fractional b, Real b) => [a] -> b
average' [] = 0
average' xs = sum' frac / count' frac
  where frac = map realToFrac xs

average'' :: (Fractional b, Real b) => [b] -> b
average'' [] = 0
average'' xs = sum' xs / count' xs 

average''' :: (Fractional b, Real b) => [b] -> b
average''' [] = 0
average''' xs = foldl wibble 0 xs 
  where lenAsFrac = realToFrac(length xs)
        wibble = (\acc x -> acc+x / lenAsFrac )

--average'''' :: (Fractional b, Real b) => [b] -> b
--average'''' [] = 0
--average'''' (x:xs) = fst (foldl wibble (x, 1.0) xs)
  --where tailAsFrac = map realToFrac xs
        --wibble = (\acc x -> (((fst acc) + (x/(snd acc))) * ((snd acc)+1) / ((snd acc)+1), ((snd acc)+1) ))


last' :: [a] -> a
last' ys = foldl(\acc x -> x) (head ys) ys

penultimate' :: [a] -> a
penultimate' ys = fst lastTwo
  where lastTwo = foldl getLastTwo (firstEl, secondEl) ys
        getLastTwo = (\acc x -> ((snd acc),x) ) 
        firstEl = head ys
        secondEl = head (tail ys)




--def average(list: List[Double]): Double = list match {
--  case head :: tail => tail.foldLeft( (head,1.0) )((r,c) =>
--    ((r._1 + (c/r._2)) * r._2 / (r._2+1), r._2+1) )._1
--  case Nil => NaN
--}




-- vre530268258
