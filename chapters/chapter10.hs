{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isAlphaNum, isSpace)
import Data.Coerce
import Data.Validation
import Data.List.NonEmpty

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Rule a = (a -> Validation Error a)

newtype Password = Password T.Text deriving (Eq, Show)
newtype Username = Username T.Text deriving (Eq, Show)
newtype Error = Error (NonEmpty T.Text) deriving (Eq, Show)

instance Semigroup Error where
    Error xs <> Error ys = Error (xs <>  ("\n" :| []) <> ys)

textToError :: T.Text -> Error
textToError text = Error (text :| [])

data User = User Username Password deriving Show

incorrectLengthMessage = "Input has incorrect length."
containsNonAlphanumericMessage = "Input contains invalid (non alphanumeric) symbols."
emptyPasswordMessage = "Input cannot be empty."

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
    case validatePassword password of
        Failure err -> Failure (textToError "Invalid password:" <> err)
        Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
    case validateUsername username of
        Failure err -> Failure (textToError "Invalid username:" <> err)
        Success username2 -> Success username2

checkLength :: Int -> Int -> T.Text -> Validation Error T.Text
checkLength minLength maxLength str =
    case ( strIsTooLong || strIsTooShort) of
        True -> Failure (textToError incorrectLengthMessage)
        False -> Success str
    where
        strLength = T.length str
        strIsTooLong = strLength > maxLength
        strIsTooShort = strLength < minLength

checkPasswordLength :: Rule Password
checkPasswordLength password =
    coerce $ checkLength 10 20 text
        where
            text = coerce @Password @T.Text password


checkUsernameLength :: Rule Username
checkUsernameLength username =
    coerce $ checkLength 10 15 text
        where
            text = coerce @Username @T.Text username

requireAlphaNum :: Rule T.Text
requireAlphaNum password =
    case (all isAlphaNum (T.unpack password)) of
        True -> Success password
        False -> Failure $ textToError containsNonAlphanumericMessage

cleanWhitespace :: Rule T.Text
cleanWhitespace "" = Failure $ textToError emptyPasswordMessage
cleanWhitespace str = Success (T.stripStart str)

validatePassword :: Rule Password
validatePassword password =
    case (coerce cleanWhitespace :: Rule Password) password of
        Failure err -> Failure err
        Success password2 -> (coerce requireAlphaNum :: Rule Password) password2 *> checkPasswordLength (coerce password2)

validateUsername :: Rule Username
validateUsername username =
    case (coerce cleanWhitespace :: Rule Username) username of
        Failure err -> Failure err
        Success username2 -> (coerce requireAlphaNum :: Rule Username) username2 *> checkUsernameLength (coerce username2)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
    User <$> usernameErrors username <*> passwordErrors password

display :: Username -> Password -> IO ()
display name password =
    case makeUser name password of
        Failure err -> T.putStr $ foldl (<>) "" (coerce err :: NonEmpty T.Text)
        Success (User username _) ->
            T.putStr ("Welcome, " <> coerce @Username @T.Text username)

main :: IO ()
main = do
    putStr "Please enter username\n> " 
    username <- Username <$> T.getLine 
    putStr "Please enter password\n> " 
    password <- Password <$> T.getLine 
    display username password

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
        eq 1 (checkPasswordLength $ Password "") (Failure $ textToError incorrectLengthMessage)
        eq 2 (checkPasswordLength $ Password "julielovesbooks") (Success $ Password "julielovesbooks")
        eq 3 (checkUsernameLength $ Username "") (Failure $ textToError incorrectLengthMessage)
        eq 4 (checkUsernameLength $ Username "julielovesbooks") (Success $ Username "julielovesbooks")
        eq 5 (requireAlphaNum "dsa123--") (Failure $ textToError containsNonAlphanumericMessage)
        eq 6 (requireAlphaNum "julielovesbooks") (Success "julielovesbooks")
        eq 7 (cleanWhitespace "") (Failure $ textToError emptyPasswordMessage)
        eq 8 (cleanWhitespace " julielovesbooks") (Success "julielovesbooks")

