{-# LANGUAGE FlexibleInstances #-}

module Linguistics.HandleIrregularPreterite
    ( HandlesIrregularPreterite
    , handleIrregularPreterite
    ) where

import Linguistics.Intermediate
import Linguistics.Types

class HandlesIrregularPreterite a where
    handleIrregularPreterite ::
           a -> Bool -> InnerSyllable' -> Intermediate -> Maybe Intermediate

instance HandlesIrregularPreterite (SubjectSensativeTense, Subject) where
    handleIrregularPreterite (Preterite, Yo) shouldReplace syllable =
        updatePenultimateSyllable shouldReplace syllable . setSimpleEnd E
    handleIrregularPreterite (Preterite, Usted) shouldReplace syllable =
        updatePenultimateSyllable shouldReplace syllable . setSimpleEnd O
    handleIrregularPreterite (Preterite, Él) a b =
        handleIrregularPreterite (Preterite, Usted) a b
    handleIrregularPreterite (Preterite, _) a b = updatePenultimateSyllable a b
    handleIrregularPreterite (ImperfectSubjunctive, _) a b =
        updatePenultimateSyllable a b
    handleIrregularPreterite _ _ _ = return

instance HandlesIrregularPreterite SubjectlessTense where
    handleIrregularPreterite _ _ _ = return
