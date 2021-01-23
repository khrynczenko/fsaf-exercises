{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Char (isAlphaNum, isSpace)

incorrectLengthMessage = "Incorrect password length."

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
    case ( passwordIsTooLong || passwordIsTooShort) of
        True -> Left incorrectLengthMessage
        False -> Right password
    where
        passwordLength = length password
        passwordIsTooLong = passwordLength > 20
        passwordIsTooShort = passwordLength < 10

containsNonAlphanumericMessage = "Password contains invalid (non alphanumeric) symbols."

requireAlphaNum :: String -> Either String String
requireAlphaNum password =
    case (all isAlphaNum password) of
        True -> Right password
        False -> Left containsNonAlphanumericMessage

emptyPasswordMessage = "Password cannot be empty."

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left emptyPasswordMessage
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Right (x:xs)

validatePassword :: String -> Either String String
validatePassword password =
    cleanWhitespace password >>= requireAlphaNum >>= checkPasswordLength

-- Exercise 14
-- We still gen only he first error that occured.

-- Exercise 15
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

-- Exercise 16
-- String is nullary kinds aka conrecte type aka nullary type constructor
-- so it cannot be a monad.
--
-- [] is a unary kind, aka unary type constructor, so it can be made as a monad
-- 
-- (,) is a binary kind so it cannot be a monad. Although (,) with one type
-- applied could be.
-- data Pair a = Pair a a is a unary kind so it can be a monad.

main :: IO ()
main = do
    putStr "Please enter password\n> "
    password <- getLine
    print (validatePassword password)

test :: IO ()
test = printTestResult $
    do
        eq 1 (checkPasswordLength "") (Left incorrectLengthMessage)
        eq 2 (checkPasswordLength "julielovesbooks") (Right "julielovesbooks")
        eq 3 (requireAlphaNum "dsa123--") (Left containsNonAlphanumericMessage)
        eq 4 (requireAlphaNum "julielovesbooks") (Right "julielovesbooks")
        eq 3 (cleanWhitespace "") (Left emptyPasswordMessage)
        eq 4 (cleanWhitespace " julielovesbooks") (Right "julielovesbooks")


