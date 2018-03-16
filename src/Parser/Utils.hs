module Parser.Utils
    ( satisfy
    , lookAhead
    , terminal
    , char
    ) where

import Parser

satisfy :: (String, Char -> Bool) -> Parser String Char
satisfy (msg, pred) =
    Parser $ \input ->
        case input of
            (c:cs)
                | pred c -> Right (c, cs)
            _ -> Left (msg ++ " at: '" ++ input ++ "'.")

lookAhead :: (Char -> Bool) -> Parser String ()
lookAhead pred =
    Parser $ \input ->
        case input of
            (c:cs)
                | pred c -> Right ((), input)
            _ -> Left ("lookAhead failed at: '" ++ input ++ "'.")

terminal :: Parser String ()
terminal =
    Parser $ \input ->
        if input == ""
            then Right ((), input)
            else Left
                     ("String did not terminate.  Remainder: '" ++
                      input ++ "'. ")

char :: Char -> Parser String Char
char c = satisfy ("Char did not match '" ++ [c] ++ "'", (==) c)
