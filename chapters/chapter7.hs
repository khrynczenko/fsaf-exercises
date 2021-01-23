{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List

newtype Password = Password String deriving (Eq, Show)
newtype Username = Username String deriving (Eq, Show)
newtype Error = Error String deriving (Eq, Show)

data User = User Username Password deriving Show

incorrectLengthMessage = "Value has incorrect length."

checkLength :: Int -> Int -> String -> Either Error String
checkLength minLength maxLength str =
    case ( strIsTooLong || strIsTooShort) of
        True -> Left (Error incorrectLengthMessage)
        False -> Right str
    where
        strLength = length str
        strIsTooLong = strLength > maxLength
        strIsTooShort = strLength < minLength

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
    cleanWhitespace password >>= requireAlphaNum >>= checkPasswordLength

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
    cleanWhitespace username >>= requireAlphaNum >>= checkUsernameLength

makeUser :: Username -> Password -> Either Error User
makeUser username password =
    --validateUsername username >>= (\username -> User username <$> (validatePassword password))
    --do
        --validUsername <- validateUsername username
        --validPassword <- validatePassword password
        --return $ User validUsername validPassword
    User <$> validateUsername username <*> validatePassword password

main :: IO ()
main = do
    putStr "Please enter username\n> " 
    username <- Username <$> getLine 
    putStr "Please enter password\n> " 
    password <- Password <$> getLine 
    print $ makeUser username password 

-- Exercise 21
--makeUserWithTemppassword :: Username -> Password -> Either Error User
--makeUserWithTemppassword username password =
    --User <$> validateUsername username <*> Password "temppassword"
-- It does not work <*> requires its second parameter to be "f a", i.e.,
-- it should be an applicative, In this case we have Password which is neither
-- a unary kind type, nro an applicative. It is a concrete type. We can fix
-- this by adding pure in order to put Password in the context of Either!

-- Exercise 22
pureMaybe :: a -> Maybe a
pureMaybe value = Just value

pureEither :: b -> Either a b
pureEither value = Right value
    
-- Exercise 23
isWord :: String -> Maybe String
isWord word =
    case (null word) of
        True -> Nothing
        False -> case (all isAlpha word) of
            True -> Just word
            False -> Nothing

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = sort word1 == sort word2

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
    case (isWord word1) of
        Nothing -> "The first word is invalid."
        Just word1 -> case (isWord word2) of
            Nothing -> "The second word is invalid."
            Just word2 -> case (isAnagram word1 word2) of
                True -> "Words are anagrams."
                False -> "Words are not anagrams."
main2 :: IO ()
main2 = do
    result <- checkAnagram <$> getLine <*> getLine
    print result

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


