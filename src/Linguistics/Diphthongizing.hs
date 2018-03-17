{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Diphthongizing
    ( Diphthongizing
    , diphthongize
    ) where

import Linguistics.Types

class Diphthongizing a where
    diphthongize :: a -> Maybe a

instance Diphthongizing Core where
    diphthongize (Nothing, Left (False, U)) =
        Just (Just U, Right ((False, E), Nothing))
    diphthongize (Nothing, Right ((False, E), Nothing)) =
        Just (Just I, Right ((False, E), Nothing))
    diphthongize (Nothing, Right ((False, O), Nothing)) =
        Just (Just U, Right ((False, E), Nothing))
    diphthongize _ = Nothing

instance Diphthongizing Stem where
    diphthongize (onset, (core, consonants):syllableTail) =
        fmap
            (\dipCore -> (onset, (dipCore, consonants) : syllableTail))
            (diphthongize core)
    diphthongize _ = Nothing

instance Diphthongizing Intermediate where
    diphthongize (s, e) = fmap (flip (,) e) (diphthongize s)
