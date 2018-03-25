{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Diphthongizing
    ( Diphthongizing
    , diphthongize
    ) where

import Linguistics.Types

class Diphthongizing a where
    diphthongize :: a -> Maybe a

instance Diphthongizing Core where
    diphthongize (False, (Nothing, Left U)) =
        Just (False, (Just U, Right (E, Nothing)))
    diphthongize (False, (Nothing, Left I)) =
        Just (False, (Just I, Right (E, Nothing)))
    diphthongize (False, (Nothing, Right (E, Nothing))) =
        Just (False, (Just I, Right (E, Nothing)))
    diphthongize (False, (Nothing, Right (O, Nothing))) =
        Just (False, (Just U, Right (E, Nothing)))
    diphthongize _ = Nothing

instance Diphthongizing Intermediate where
    diphthongize (vt, onset, (core, consonants):syllableTail, ending) =
        fmap
            (\dipCore ->
                 (vt, onset, (dipCore, consonants) : syllableTail, ending))
            (diphthongize core)
    diphthongize _ = Nothing
