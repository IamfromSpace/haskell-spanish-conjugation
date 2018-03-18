{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.VowelRaising
    ( VowelRaising
    , raiseVowel
    ) where

import Linguistics.Types

class VowelRaising a where
    raiseVowel :: a -> Maybe a

instance VowelRaising Core where
    raiseVowel (False, (Nothing, Right (E, Nothing))) =
        Just (False, (Nothing, Left I))
    raiseVowel (False, (Nothing, Right (O, Nothing))) =
        Just (False, (Nothing, Left U))
    raiseVowel _ = Nothing

instance VowelRaising Stem where
    raiseVowel (onset, (core, consonants):syllableTail) =
        fmap
            (\raisedCore -> (onset, (raisedCore, consonants) : syllableTail))
            (raiseVowel core)
    raiseVowel _ = Nothing

instance VowelRaising Intermediate where
    raiseVowel (s, e) = fmap (flip (,) e) (raiseVowel s)
