module Linguistics.Conjugate
    ( conjugate
    ) where

import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.Intermediate
import Linguistics.Types
import Linguistics.VowelRaising

-- TODO: Could use some tightening up here for when to work with verbs/words
-- (both here and in the intermediate module).
-- This will likely go through some major changes though when this actually
-- looks up the ending from the verb, rather than being supplied an ending.
conjugate :: Bool -> Bool -> FullWord -> Ending -> Maybe FullWord
conjugate a b word ending = toVerb word >>= flip (conjugate' a b) ending

conjugate' :: Bool -> Bool -> Verb -> Ending -> Maybe FullWord
conjugate' diphthongizing vowelRaising verb ending =
    let intermediate =
            preventUirNonIDiphthongization (toIntermediate' verb ending)
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
