-- read in file containing monkey island shiz
-- whack it in an array of tuples
-- find comeback in array by locating tuple containing insult
-- where tuples are like (insult, comeback)
-- if input matches comeback, then boom!


main =
	let x = "hey"
	in do
	  putStrLn x
	  comeback <- getLine
	  putStrLn $ show (isRight x comeback)

comeback :: String -> String
comeback x = "hey"

isRight :: String -> String -> Bool
isRight insult comeback = 
  comeback == rightComeback
  where rightComeback = findFirstMatching insult

findFirstMatching :: String -> String
findFirstMatching insult = snd (head foo)
  where foo = filter (\x -> fst x == insult) eek
        eek = [("hey", "ho"), ("oggi", "oi")]