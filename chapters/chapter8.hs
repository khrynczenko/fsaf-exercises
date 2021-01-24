{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Char (isAlphaNum, isSpace)
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Password = Password T.Text deriving (Eq, Show)
newtype Username = Username T.Text deriving (Eq, Show)
newtype Error = Error [T.Text] deriving (Eq, Show)

instance Semigroup Error where
    Error xs <> Error ys = Error (xs <> ys)

--instance Monad (Validation e) where
    --Success 

data User = User Username Password deriving Show

incorrectLengthMessage = "Input has incorrect length."
containsNonAlphanumericMessage = "Input contains invalid (non alphanumeric) symbols."

checkLength :: Int -> Int -> T.Text -> Validation Error T.Text
checkLength minLength maxLength str =
    case ( strIsTooLong || strIsTooShort) of
        True -> Failure (Error [incorrectLengthMessage])
        False -> Success str
    where
        strLength = T.length str
        strIsTooLong = strLength > maxLength
        strIsTooShort = strLength < minLength

checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength password =
    fmap Password $ checkLength 10 20 password


checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength username =
    fmap Username $ checkLength 10 15 username

requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum password =
    case (all isAlphaNum (T.unpack password)) of
        True -> Success password
        False -> Failure $ Error [containsNonAlphanumericMessage]

emptyPasswordMessage = "Input cannot be empty."

cleanWhitespace :: T.Text -> Validation Error T.Text
cleanWhitespace "" = Failure $ Error [emptyPasswordMessage]
cleanWhitespace str = Success (T.stripStart str)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) = do
    case cleanWhitespace password of
        Failure err -> Failure err
        Success password2 -> requireAlphaNum password2 *> checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) = do
    case cleanWhitespace username of
        Failure err -> Failure err
        Success username2 -> requireAlphaNum username2 *> checkUsernameLength username2

-- Exercise 24
-- It could because Either is also an Applicative but the effect would be the
-- same, again we would get only one of the errors.

-- Exercise 25
promptWord1 :: IO T.Text
promptWord1 =
    T.putStr "Please enter a word. \n> " *> T.getLine

-- Exercise 26
-- We use do in context of an Applicative due to ApplciativeDo extension
makeUserDo :: Username -> Password -> Validation Error User
makeUserDo username password = do
    validatedUsername <- validateUsername username
    validatedPassword <- validatePassword password
    pure $ User validatedUsername validatedPassword

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
    User <$> validateUsername username <*> validatePassword password

main :: IO ()
main = do
    putStr "Please enter username\n> " 
    username <- Username <$> T.getLine 
    putStr "Please enter password\n> " 
    password <- Password <$> T.getLine 
    print $ makeUser username password 

printTestResult :: Either T.Text () -> IO ()
printTestResult result =
    case result of
        Left err -> T.putStrLn err
        Right () -> T.putStrLn "All tests have passed"

eq :: (Eq a, Show a) => Int -> a -> a -> Either T.Text ()
eq n actual expected =
    case (actual == expected) of
        True -> Right ()
        False -> Left (T.unlines
            [ T.pack ("Test " ++ show n)
            , T.pack (" Expected: " ++ show expected)
            , T.pack (" But Got:  " ++ show actual)
            ])

test :: IO ()
test = printTestResult $
    do
        eq 1 (checkPasswordLength $ "") (Failure $ Error [incorrectLengthMessage])
        eq 2 (checkPasswordLength $ "julielovesbooks") (Success $ Password "julielovesbooks")
        eq 3 (checkUsernameLength $ "") (Failure $ Error [incorrectLengthMessage])
        eq 4 (checkUsernameLength $ "julielovesbooks") (Success $ Username "julielovesbooks")
        eq 5 (requireAlphaNum "dsa123--") (Failure $ Error [containsNonAlphanumericMessage])
        eq 6 (requireAlphaNum "julielovesbooks") (Success "julielovesbooks")
        eq 7 (cleanWhitespace "") (Failure $ Error [emptyPasswordMessage])
        eq 8 (cleanWhitespace " julielovesbooks") (Success "julielovesbooks")

