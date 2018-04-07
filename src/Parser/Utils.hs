module Parser.Utils
    ( satisfy
    , lookAhead
    , terminal
    , char
    ) where

import Parser

satisfy :: (String, Char -> Bool) -> Parser String Char
satisfy (msg, predicate) =
    Parser $ \input ->
        case input of
            (c:cs)
                | predicate c -> Right (cs, c)
            _ -> Left (msg ++ " at: '" ++ input ++ "'.")

lookAhead :: (Char -> Bool) -> Parser String ()
lookAhead predicate =
    Parser $ \input ->
        case input of
            (c:_)
                | predicate c -> Right (input, ())
            _ -> Left ("lookAhead failed at: '" ++ input ++ "'.")

terminal :: Parser String ()
terminal =
    Parser $ \input ->
        if input == ""
            then Right (input, ())
            else Left
                     ("String did not terminate.  Remainder: '" ++
                      input ++ "'. ")

char :: Char -> Parser String Char
char c = satisfy ("Char did not match '" ++ [c] ++ "'", (==) c)
