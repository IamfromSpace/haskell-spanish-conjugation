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

instance Diphthongizing Stem where
    diphthongize (onset, (core, consonants):syllableTail) =
        fmap
            (\dipCore -> (onset, (dipCore, consonants) : syllableTail))
            (diphthongize core)
    diphthongize _ = Nothing

instance Diphthongizing Intermediate where
    diphthongize (s, e) = fmap (flip (,) e) (diphthongize s)
