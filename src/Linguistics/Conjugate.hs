module Linguistics.Conjugate
    ( conjugate
    ) where

import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.Intermediate
import Linguistics.Types
import Linguistics.VowelRaising

conjugate :: Bool -> Bool -> FullWord -> Ending -> Maybe FullWord
conjugate diphthongizing vowelRaising word ending =
    let intermediate =
            preventUirNonIDiphthongization (toIntermediate word ending)
        mIntermediate =
            if diphthongizing && couldDiphthongize intermediate
                then diphthongize intermediate
                else Just intermediate
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
