{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Char (isAlphaNum, isSpace)

newtype Password = Password String deriving (Eq, Show)
newtype Username = Username String deriving (Eq, Show)
newtype Error = Error String deriving (Eq, Show)

incorrectLengthMessage = "Value has incorrect length."

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
    fmap Password $ checkLength 10 20 password


checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
    fmap Username $ checkLength 10 15 username

containsNonAlphanumericMessage = "Password contains invalid (non alphanumeric) symbols."

requireAlphaNum :: String -> Either Error String
requireAlphaNum password =
    case (all isAlphaNum password) of
        True -> Right password
        False -> Left $ Error containsNonAlphanumericMessage

emptyPasswordMessage = "Password cannot be empty."

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left $ Error emptyPasswordMessage
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Right (x:xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) = do
    cleanedPassword <- cleanWhitespace password
    alphanumericPassword <- requireAlphaNum cleanedPassword
    checkPasswordLength alphanumericPassword

-- Exercise 17
validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
    cleanWhitespace username >>= requireAlphaNum >>= checkUsernameLength

-- Exercise 18
checkLength :: Int -> Int -> String -> Either Error String
checkLength minLength maxLength str =
    case ( strIsTooLong || strIsTooShort) of
        True -> Left (Error incorrectLengthMessage)
        False -> Right str
    where
        strLength = length str
        strIsTooLong = strLength > maxLength
        strIsTooShort = strLength < minLength

-- Exercise 19
main :: IO ()
main = do
    putStr "Please enter password\n> " 
    >> getLine 
    >>= (print . validatePassword . Password)

printTestResult :: Either String () -> IO ()
printTestResult result =
    case result of
        Left err -> putStrLn err
        Right () -> putStrLn "All tests have passed"

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
    case (actual == expected) of
        True -> Right ()
        False -> Left (unlines
            [ "Test " ++ show n
            , " Expected: " ++ show expected
            , " But Got:  " ++ show actual
            ])
test :: IO ()
test = printTestResult $
    do
        eq 1 (checkPasswordLength $ "") (Left $ Error incorrectLengthMessage)
        eq 2 (checkPasswordLength $ "julielovesbooks") (Right $ Password "julielovesbooks")
        eq 3 (checkUsernameLength $ "") (Left $ Error incorrectLengthMessage)
        eq 4 (checkUsernameLength $ "julielovesbooks") (Right $ Username "julielovesbooks")
        eq 5 (requireAlphaNum "dsa123--") (Left $ Error containsNonAlphanumericMessage)
        eq 6 (requireAlphaNum "julielovesbooks") (Right "julielovesbooks")
        eq 7 (cleanWhitespace "") (Left $ Error emptyPasswordMessage)
        eq 8 (cleanWhitespace " julielovesbooks") (Right "julielovesbooks")


