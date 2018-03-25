module Linguistics.Conjugate
    ( conjugate
    ) where

import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.Intermediate
import Linguistics.Types
import Linguistics.VerbEnding
import Linguistics.VowelRaising

-- TODO: this function struture of `Bool -> Bool -> a -> b -> c` is really troublesome to map
-- And it's only going to get worse as verbs get more properties (diphthong-breaking,
-- custom preterite/subjunctives, etc)
conjugate :: Bool -> Bool -> Verb -> SimpleTense -> Maybe FullWord
conjugate diphthongizing vowelRaising verb@(vt, _, _, _) tense =
    let ending = getEnding vt tense
        intermediate =
            preventUirNonIDiphthongization (toIntermediate verb ending)
        mIntermediate =
            if diphthongizing && couldDiphthongize intermediate
                then diphthongize intermediate
                else return intermediate
        mIntermediate' =
            if vowelRaising && couldVowelRaise diphthongizing intermediate
                then mIntermediate >>= raiseVowel
                else mIntermediate
        mIntermediate'' =
            fmap
                (preventStressedJointDiphthongization .
                 preventAmbiguiousJointDiphthongization)
                mIntermediate'
        fullWord =
            fmap
                (dropSemiVowelIAfterÑ .
                 dropSemiVowelIAfterLl .
                 dropMonosyllabicAccent .
                 preventStartingSemiVowel . fromIntermediate)
                mIntermediate''
    in fullWord
