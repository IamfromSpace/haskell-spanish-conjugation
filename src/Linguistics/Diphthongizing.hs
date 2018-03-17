{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Diphthongizing
    ( Diphthongizing
    , diphthongize
    ) where

import Linguistics.Types

class Diphthongizing a where
    diphthongize :: a -> Maybe a

instance Diphthongizing Core where
    diphthongize (Nothing, (False, Left U)) =
        Just (Just U, (False, Right (E, Nothing)))
    diphthongize (Nothing, (False, Right (E, Nothing))) =
        Just (Just I, (False, Right (E, Nothing)))
    diphthongize (Nothing, (False, Right (O, Nothing))) =
        Just (Just U, (False, Right (E, Nothing)))
    diphthongize _ = Nothing

instance Diphthongizing Stem where
    diphthongize (onset, (core, consonants):syllableTail) =
        fmap
            (\dipCore -> (onset, (dipCore, consonants) : syllableTail))
            (diphthongize core)
    diphthongize _ = Nothing

instance Diphthongizing Intermediate where
    diphthongize (s, e) = fmap (flip (,) e) (diphthongize s)
