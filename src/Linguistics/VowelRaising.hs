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

instance VowelRaising Intermediate where
    raiseVowel (vt, onset, (core, consonants):syllableTail, ending) =
        fmap
            (\raisedCore ->
                 (vt, onset, (raisedCore, consonants) : syllableTail, ending))
            (raiseVowel core)
    raiseVowel _ = Nothing
