-- read in file containing monkey island shiz
-- whack it in an array of tuples
-- find comeback in array by locating tuple containing insult
-- where tuples are like (insult, comeback)
-- if input matches comeback, then boom!

-- TODO readlines in with 
--     handle <- openFile "girlfriend.txt" ReadMode  
-- contents <- hGetContents handle  
--    putStr contents  
--    hClose handle  
-- split lines with Prelude.lines
-- then take odd lines as insults (or leave it the way it comes from the MI page with insult prefiX LDLD)
-- and the other lines as comebacks
-- actually would be fucktonnes easier to filter with the prefixes left in - do that 



-- say a thing
-- get user input
-- if user input matches, go back to start
-- else quit	

import Control.Monad
import System.Random

main = do
    gen <- getStdGen
    let insultPairs = [
         ("This is the END for you, you gutter-crawling cur!", "And I've got a little TIP for you, get the POINT?"), 
         ("Soon you'll be wearing my sword like a shish kebab!", "First you better stop waiving it like a feather-duster."),
         ("My handkerchief will wipe up your blood!", "So you got that job as janitor, after all.")
         ]
    putStrLn "Insult Sword Fighting"
    insultMe gen insultPairs


--getCombos :: [(String, String)]
--getCombos = 
--	let contents <- hGetContents handle
--    let handle <-  openFile "mi_insults.txt" ReadMode



insultMe :: StdGen -> [(String, String)] -> IO ()
insultMe gen insultPairs = do
    -- putStrLn $ show gen
    let len = (length insultPairs) -1
    if len >= 0 
        then do
            let (xy, newGen) = randomR (0, len) gen :: (Int, StdGen)
            -- putStrLn $ "randomly picked insult index " ++ show xy
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
            putStrLn "You're good enough to fight the sword master! <3" 
            return ()

isRight :: String -> String -> [(String, String)]-> Bool
isRight insult comeback insultPairs = 
  comeback == rightComeback
  where rightComeback = findFirstMatching insult insultPairs

findFirstMatching :: String -> [(String, String)] -> String
findFirstMatching insult insultPairs = snd (head foo)
  where foo = filter (\x -> fst x == insult) insultPairs