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
    case cleanWhitespace password of
        Nothing -> Nothing
        Just cleanedPassword ->
            case requireAlphaNum cleanedPassword of
                Nothing -> Nothing
                Just alphanumPassword ->
                    case checkPasswordLength alphanumPassword of
                        Nothing -> Nothing
                        Just checkedForLengthPassword -> Just checkedForLengthPassword
            

main :: IO ()
main = do
    putStr "Please enter password\n> "
    password <- getLine
    print (validatePassword password)
