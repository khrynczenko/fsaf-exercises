{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Char (isAlphaNum, isSpace)

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
    case ( passwordIsTooLong || passwordIsTooShort) of
        True -> Nothing
        False -> Just password
    where
        passwordLength = length password
        passwordIsTooLong = passwordLength > 20
        passwordIsTooShort = passwordLength < 10

requireAlphaNum :: String -> Maybe String
requireAlphaNum password =
    case (all isAlphaNum password) of
        True -> Just password
        False -> Nothing

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Just (x:xs)

validatePassword :: String -> Maybe String
validatePassword password =
    cleanWhitespace password >>= requireAlphaNum >>= checkPasswordLength

-- Exercise 10
reverseLine :: IO ()
--reverseLine = getLine >>= (print . reverse)
reverseLine = do
    line <- getLine
    (print . reverse) line
           
-- Exercise 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe (Just x) f = f x
bindMaybe Nothing _ = Nothing

-- Exercise 12
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str string) f = Str string
bindStringOrValue (Val x) f = f x

main :: IO ()
main = do
    putStr "Please enter password\n> "
    password <- getLine
    print (validatePassword password)
