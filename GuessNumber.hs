module GuessNumber where

import System.Random
import Text.Read (readMaybe)

readGuess :: IO Int
readGuess = do
    q <- getLine
    case readMaybe q of
        Just x -> return x
        Nothing -> do
            putStr "Invalid number, try again: "
            readGuess

minSecret = 1
maxSecret = 100
attemptsCount = 10

step :: Int -> Int -> IO ()
step secret 0 = putStrLn $ "You lose! Secret number was " ++ (show secret) ++ "."
step secret tries = do
    putStr $ "You have " ++ (show tries) ++ " attempt(s). Enter your guess (" ++ (show minSecret) ++ " - " ++ (show maxSecret) ++ "): "
    guess <- readGuess
    if secret == guess
        then putStrLn $ "You are right! My number is indeed " ++ (show secret) ++ "."
        else do
            let cmp = if secret > guess then "bigger" else "smaller"
            putStrLn $ "Nope. My number is " ++ cmp ++ "."
            putStrLn ""
            step secret (tries - 1)

guessNumber :: IO ()
guessNumber = do
    secret <- randomRIO (minSecret, maxSecret)
--    putStrLn $ "Secret is " ++ (show secret) ++ "."
    step secret attemptsCount
