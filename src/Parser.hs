module Parser
    ( Parser(..)
    ) where

import Control.Applicative

-- Note that while this seems like this could just be
-- the Compose of three functors, that reaaaaally doesn't work.
-- The instances are far to specific for defaults to work,
-- and then ((->) a) and (Either a) are not Alternatives
-- which causes a whole bunch of problems.
newtype Parser i o = Parser
    { runParser :: i -> Either String (i, o)
    }

instance Functor (Parser i) where
    fmap f = Parser . fmap (fmap (fmap f)) . runParser

instance Applicative (Parser i) where
    pure x = Parser $ \a -> Right (a, x)
    pf <*> px =
        Parser $ \input ->
            case runParser pf input of
                Left e -> Left e
                Right (rest, f) -> fmap (fmap f) (runParser px rest)

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
