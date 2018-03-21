module Linguistics.FullWord
    ( preventStartingSemiVowel
    , dropMonosyllabicAccent
    , dropSemiVowelIAfterÑ
    , dropSemiVowelIAfterLl
    ) where

import Control.Arrow as Arrow
import Linguistics.Types

-- Can't start a word with a semi-vowel, so we upgrade it or add an h
preventStartingSemiVowel :: FullWord -> FullWord
preventStartingSemiVowel word@(Nothing, (_, (Just U, _)), _, _) =
    mapMaybeOnset (const (Just (Single (Regular H)))) word
preventStartingSemiVowel word@(Nothing, (_, (Just I, _)), _, _) =
    let word' = mapFirstCore dropInitialSemiVowel word
    in mapMaybeOnset (const (Just (Single (Regular Y)))) word'
preventStartingSemiVowel x = x

-- Logically, there is no reason to explicitly stress the only syllable
dropMonosyllabicAccent :: FullWord -> FullWord
dropMonosyllabicAccent word@(_, (True, _), [], _) =
    mapFirstCore (Arrow.first (const False)) word
dropMonosyllabicAccent x = x

-- Drop 'i' as a semi-vowel after 'll' since it adds no sound
-- We need this rule here, in the off chance that either the ending OR
-- a diphthongization created a semi-vowel i that triggers it.
dropSemiVowelIAfterLl' :: InnerSyllable -> InnerSyllable
dropSemiVowelIAfterLl' (cluster@(Just (Just (Coda False (Liquid L)), Single (Liquid L))), core@(_, (Just I, _))) =
    (cluster, dropInitialSemiVowel core)
dropSemiVowelIAfterLl' x = x

dropSemiVowelIAfterLl :: FullWord -> FullWord
dropSemiVowelIAfterLl = mapInnerSyllables dropSemiVowelIAfterLl'

-- Drop 'i' as a semi-vowel after 'ñ' since it adds no sound
-- This rule is like the previous but a little trickier, in that this can
-- apply to the first core, where as the previous cannot
-- (because 'll' doesn't fit inside just an onset, and ñ does)
-- We need this rule here, in the off chance that either the ending OR
-- a diphthongization created a semi-vowel i that triggers it.
dropSemiVowelIAfterÑ'' :: Maybe Onset -> Core -> Core
dropSemiVowelIAfterÑ'' (Just (Single (Regular Ñ))) core@(_, (Just I, _)) =
    dropInitialSemiVowel core
dropSemiVowelIAfterÑ'' _ x = x

dropSemiVowelIAfterÑ' :: InnerSyllable -> InnerSyllable
dropSemiVowelIAfterÑ' (cluster, core) =
    (cluster, dropSemiVowelIAfterÑ'' (fmap snd cluster) core)

dropSemiVowelIAfterÑ :: FullWord -> FullWord
dropSemiVowelIAfterÑ word@(mOnset, _, _, _) =
    let word' = mapFirstCore (dropSemiVowelIAfterÑ'' mOnset) word
    in mapInnerSyllables dropSemiVowelIAfterÑ' word'

--HELPERS
mapMaybeOnset :: (Maybe Onset -> Maybe Onset) -> FullWord -> FullWord
mapMaybeOnset f (mOnset, core, innerSyllables, mCore) =
    (f mOnset, core, innerSyllables, mCore)

mapInnerSyllables :: (InnerSyllable -> InnerSyllable) -> FullWord -> FullWord
mapInnerSyllables f (mOnset, core, innerSyllables, mCore) =
    (mOnset, core, fmap f innerSyllables, mCore)

mapFirstCore :: (Core -> Core) -> FullWord -> FullWord
mapFirstCore f (mOnset, core, innerSyllables, mCore) =
    (mOnset, f core, innerSyllables, mCore)

--TODO:  This should move to a Core module with related helpers
dropInitialSemiVowel :: Core -> Core
dropInitialSemiVowel (isAccented, (Just _, v)) = (isAccented, (Nothing, v))
dropInitialSemiVowel x = x
