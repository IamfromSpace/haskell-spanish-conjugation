module Parser
    ( Parser(..)
    ) where

import Control.Applicative
import qualified Control.Arrow as Arrow

-- If we swap o/i in the tuple and constrain Monoid i => i
-- then this is just the composition (h (g (f a))) of three Applicatives
--   (->) a  - Reader or (a -> ...)
--   Maybe a
--   Monoid b => (b, a)
-- However, only Maybe appears to be an Alternative,
-- So that needs a custom definition
-- It would be nice to parameterize the error type, but
-- then it's annoying to make add a separator
newtype Parser i o = Parser
    { runParser :: i -> Either String (o, i)
    }

instance Functor (Parser i) where
    fmap f p = Parser $ \input -> fmap (Arrow.first f) (runParser p input)

instance Applicative (Parser i) where
    pure x = Parser $ \a -> Right (x, a)
    pf <*> px =
        Parser $ \input ->
            case runParser pf input of
                Left e -> Left e
                Right (f, rest) ->
                    case runParser px rest of
                        Left e -> Left e
                        Right (x, o) -> Right (f x, o)

instance Monoid i => Alternative (Parser i) where
    empty = Parser (const (Left mempty))
    (<|>) p0 p1 =
        Parser $ \input ->
            case runParser p0 input of
                Left e0 ->
                    case runParser p1 input of
                        Left e1 -> Left (e0 ++ "\n" ++ e1)
                        secondResult -> secondResult
                firstResult -> firstResult
