-- read in file containing monkey island shiz
-- whack it in an array of tuples
-- find comeback in array by locating tuple containing insult
-- where tuples are like (insult, comeback)
-- if input matches comeback, then boom!



-- say a thing
-- get user input
-- if user input matches, go back to start
-- else quit	

import Control.Monad
import System.Random

main = do
	gen <- getStdGen
	let insultPairs = [("hey", "ho"), ("oggi", "oi"), ("foo", "bar")]
	insultMe gen insultPairs


insultMe :: StdGen -> [(String, String)] -> IO ()
insultMe gen insultPairs = do
	-- putStrLn $ show gen
	let len = (length insultPairs) -1
	if len >= 0 
		then do
			let (xy, newGen) = randomR (0, len) gen :: (Int, StdGen)
			putStrLn $ show xy
			let insult = fst $ insultPairs !! xy

			putStrLn insult
			comeback <- getLine
			if isRight insult comeback insultPairs
				then do
				  -- delete (insult, comeback) insultPairs
				  -- putStrLn $ show insultPairs
			      insultMe newGen (filter (/= (insult, comeback)) insultPairs)
			    else return ()
			-- where insultPairs = [("hey", "ho"), ("oggi", "oi"), ("foo", "bar")]
		else do
			putStrLn "You win!" 
			return ()

isRight :: String -> String -> [(String, String)]-> Bool
isRight insult comeback insultPairs = 
  comeback == rightComeback
  where rightComeback = findFirstMatching insult insultPairs

findFirstMatching :: String -> [(String, String)] -> String
findFirstMatching insult insultPairs = snd (head foo)
  where foo = filter (\x -> fst x == insult) insultPairs