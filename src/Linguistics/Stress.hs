{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Stress
    ( ExplicitlyStressable
    , hasExplicitStress
    ) where

import Linguistics.Types

class ExplicitlyStressable a where
    hasExplicitStress :: a -> Bool

instance ExplicitlyStressable Core where
    hasExplicitStress (b, _, _) = b

instance ExplicitlyStressable Ending where
    hasExplicitStress (joint, endingSyllables, _) =
        hasExplicitStress joint || any (hasExplicitStress . snd) endingSyllables

instance ExplicitlyStressable Intermediate where
    hasExplicitStress (_, ending)
      -- this is a bit of an optimization, theoretically a stem can _never_ have
      -- an explicit accent (otherwise you could end up with two), so we never check it.
     = hasExplicitStress ending
