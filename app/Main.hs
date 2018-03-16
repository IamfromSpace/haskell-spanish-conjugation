module Main where

import Control.Applicative (pure)
import qualified Linguistics.Parsers as LP
import qualified Parser as P

main :: IO String
main = pure (show (P.runParser LP.wordOnly "esternocleidooccipitomastoideos"))
