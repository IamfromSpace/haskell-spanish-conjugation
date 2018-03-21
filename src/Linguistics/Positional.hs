{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
  MultiParamTypeClasses #-}

module Linguistics.Positional
    ( startsWith
    , endsWith
    ) where

import Linguistics.Types

class CanStartWith a b where
    startsWith :: a -> b -> Bool

instance CanStartWith Liquid Coda where
    startsWith R (Coda _ (Liquid R)) = True
    startsWith L (Coda _ (Liquid L)) = True
    startsWith _ _ = False

instance CanStartWith Liquid Onset where
    startsWith R (Single (Liquid R)) = True
    startsWith L (Single (Liquid L)) = True
    startsWith _ _ = False

instance CanStartWith Liquid InnerCluster where
    startsWith x (Just (Nothing, onset)) = startsWith x onset
    startsWith x (Just (Just coda, _)) = startsWith x coda
    startsWith _ _ = False

instance CanStartWith HighVowel Core where
    startsWith I (_, (Just I, _)) = True
    startsWith I (_, (Nothing, Left I)) = True
    startsWith U (_, (Just U, _)) = True
    startsWith U (_, (Nothing, Left U)) = True
    startsWith _ _ = False

instance CanStartWith HighVowel FullWord where
    startsWith x (Nothing, core, _, _) = startsWith x core
    startsWith _ _ = False

instance CanStartWith LowVowel Core where
    startsWith A (_, (Nothing, Right (A, _))) = True
    startsWith E (_, (Nothing, Right (E, _))) = True
    startsWith O (_, (Nothing, Right (O, _))) = True
    startsWith _ _ = False

class EndsWith a b where
    endsWith :: a -> b -> Bool

-- This one is a little awkward, in that things like G/K
-- can have variants on how they are created, but this totally
-- ignores that, even though you do have to supply a _complete_ G/K
instance EndsWith StopOrF Onset where
    endsWith B (Single (StopOrF B)) = True
    endsWith D (Single (StopOrF D)) = True
    endsWith F (Single (StopOrF F)) = True
    endsWith (G _) (Single (StopOrF (G _))) = True
    endsWith (K _) (Single (StopOrF (K _))) = True
    endsWith P (Single (StopOrF P)) = True
    endsWith T (Single (StopOrF T)) = True
    endsWith _ _ = False

instance EndsWith StopOrF InnerCluster where
    endsWith x (Just (_, onset)) = endsWith x onset
    endsWith _ _ = False
