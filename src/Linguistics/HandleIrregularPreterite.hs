{-# LANGUAGE FlexibleInstances #-}

module Linguistics.HandleIrregularPreterite
    ( PossibleIrregularPreteriteEffect
    , getIrregularPreteriteEffect
    ) where

import Linguistics.Types

makeSimpleEnding :: LowVowel -> Ending
makeSimpleEnding lowVowel =
    ((False, (Nothing, Right (lowVowel, Nothing))), [], Nothing)

class PossibleIrregularPreteriteEffect a where
    getIrregularPreteriteEffect :: a -> IrregularPreteriteEffect

instance PossibleIrregularPreteriteEffect (SubjectSensativeTense, Subject) where
    getIrregularPreteriteEffect (Preterite, Yo) =
        Just (Just (makeSimpleEnding E))
    getIrregularPreteriteEffect (Preterite, Usted) =
        Just (Just (makeSimpleEnding O))
    getIrregularPreteriteEffect (Preterite, Él) = Just Nothing
    getIrregularPreteriteEffect (Preterite, _) = Just Nothing
    getIrregularPreteriteEffect (ImperfectSubjunctive, _) = Just Nothing
    getIrregularPreteriteEffect _ = Nothing

instance PossibleIrregularPreteriteEffect SubjectlessTense where
    getIrregularPreteriteEffect _ = Nothing
