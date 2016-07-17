module Bot (main) where
import System.Random (randomIO, mkStdGen, StdGen, randomR)
import Data.Char (toLower)

data BotState = Empty
              | BotState Int

botReply :: BotState -> StdGen -> String -> String
botLearn :: BotState -> String -> BotState

botReply Empty rg _ = db_initial !! r
                      where r = fst $ randomR (0, length db_initial - 1) rg :: Int
botReply (BotState _) rg s
  | null s || any (== s) db_quit = rTxt db_final
  | isAnyMatch = rTxt $ snd $ head $ firstMatch
  | otherwise = rTxt db_unknown
  where rTxt txts = txts !! rIdx txts
        rIdx txts = fst $ randomR (0, length txts - 1) rg :: Int
        normalize = words . map toLower
        isAnyMatch = not $ null firstMatch
        firstMatch = filter (\p -> any (fst p) $ normalize s) db_patts

botLearn Empty _ = BotState 0
botLearn (BotState x) _ = BotState (x + 1)

botRei :: BotState -> StdGen -> String -> (String, BotState)
botRei state rs line = (line', state')
  where line' = botReply state rs line
        state' = botLearn state line

main :: IO ()
main = do
  r <- randomIO :: IO Int
  let (line, state) = botRei Empty (mkStdGen r) ""
  putStrLn line
  reiLoop state
  where reiLoop state = do
          line <- getLine
          r <- randomIO :: IO Int
          let rg = mkStdGen r
              (line', state') = botRei state rg line
          if null line || any (== line) db_quit
            then do putStrLn line'
                    return ()
            else do putStrLn line'
                    reiLoop state'
db_initial :: [String]
db_initial = ["How do you do.  Please tell me your problem.",
              "Hello, I am a computer program.",
              "Please tell me what's been bothering you.",
              "Is something troubling you?"]

db_final :: [String]
db_final = ["Goodbye.  It was nice talking to you.",
            "Goodbye.  I hope you found this session helpful.",
            "I think you should talk to a REAL analyst.  Ciao!",
            "Life is tough.  Hang in there!"]

db_quit :: [String]
db_quit = ["bye", "goodbye", "done", "exit", "quit"]

db_patts :: [((String -> Bool), [String])]
db_patts = [((== "computer"), ["Do computers worry you?",
                               "Why do you mention computers?",
                               "What do you think machines have to do with your problem?",
                               "Don't you think computers can help people?",
                               "What about machines worries you?",
                               "What do you think about machines?"]),
            ((== "dream"), ["Really?",
                            "Have you ever fantasized while you were awake?",
                            "Have you ever dreamed before?"]),
            ((== "sorry"), ["Please don't apologise.",
                            "Apologies are not necessary.",
                            "I've told you that apologies are not required.",
                            "It did not bother me.  Please continue."]),
            ((== "different"), ["How is it different?",
                                "What differences do you see?",
                                "What does that difference suggest to you?",
                                "What other distinctions do you see?",
                                "What do you suppose that disparity means?",
                                "Could there be some connection, do you suppose?",
                                "How?"]),
            ((\x -> any (== x) ["fuck", "fucker", "shit", "damn", "shut"]),
              ["Does it make you feel strong to use that kind of language?",
                             "Are you venting your feelings now?",
                             "Are you angry?",
                             "Does this topic make you feel angry?",
                             "Is something making you feel angry?",
                             "Does using that kind of language make you feel better?"])]

db_unknown :: [String]
db_unknown = ["I'm not sure I understand you fully.",
              "Please go on.",
              "That is interesting. Please continue.",
              "Tell me more about that.",
              "Does talking about this bother you?"]

