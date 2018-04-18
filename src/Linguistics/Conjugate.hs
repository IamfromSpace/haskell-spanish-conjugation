{-# LANGUAGE FlexibleInstances #-}

module Linguistics.Conjugate
    ( conjugate
    , CanConjugate
    ) where

import Control.Monad ((>=>))
import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.HandleIrregularPreterite
import Linguistics.Intermediate
import qualified Linguistics.Parsers as LP
import Linguistics.Types
import Linguistics.VerbEnding
import Linguistics.VowelRaising
import qualified Parser as P
import Utils (swap, withLeft)

-- Has to be restricted to a string?
class CanConjugate a where
    conjugate ::
           (HasVerbEnding b, HandlesIrregularPreterite b)
        => a
        -> b
        -> Either String FullWord

instance CanConjugate (VerbConfig, Verb) where
    conjugate ((irregularPreterite, hasIrregularInfinitives, isYoGoVerb, isZcVerb, isDiphthongBreaking, isDiphthongizing, isVowelRaising), verb@(vt, _, _, _)) tense =
        let ending = getEnding vt tense
            intermediate =
                preventUirNonIDiphthongization (toIntermediate verb ending)
            willYoGo = isYoGoVerb && couldYoGo intermediate
            willDiphthongize =
                isDiphthongizing &&
                couldDiphthongize intermediate && not willYoGo
            willVowelRaise =
                isVowelRaising &&
                couldVowelRaise intermediate && not willDiphthongize
            mIntermediate =
                if isDiphthongBreaking
                    then breakDiphthong intermediate
                    else return intermediate
            mIntermediate' =
                if willDiphthongize
                    then mIntermediate >>= diphthongize
                    else mIntermediate
            mIntermediate'' =
                if willVowelRaise
                    then mIntermediate' >>= raiseVowel
                    else mIntermediate'
            mIntermediate''' =
                if isZcVerb && couldCToZc intermediate
                    then mIntermediate'' >>= cToZc
                    else mIntermediate''
            mIntermediate'''' =
                if willYoGo
                    then mIntermediate''' >>= yoGo
                    else mIntermediate'''
            mIntermediate''''' =
                if hasIrregularInfinitives &&
                   couldShortenedInfinitives intermediate
                    then mIntermediate'''' >>= shortenedInfinitives
                    else mIntermediate''''
            mIntermediate'''''' =
                case irregularPreterite of
                    Just (shouldReplace, syllable) ->
                        mIntermediate''''' >>=
                        handleIrregularPreterite tense shouldReplace syllable
                    Nothing -> mIntermediate'''''
            mIntermediate''''''' =
                fmap
                    (preventStressedJointDiphthongization .
                     preventAmbiguiousJointDiphthongization)
                    mIntermediate''''''
            fullWord =
                fmap
                    (dropSemiVowelIAfterÑ .
                     dropSemiVowelIAfterLl .
                     dropMonosyllabicAccent .
                     preventStartingSemiVowel . fromIntermediate)
                    mIntermediate'''''''
        in withLeft "could not conjugate" fullWord

instance CanConjugate (VerbConfig, String) where
    conjugate fwv st =
        let toVerb' = withLeft "word is not a verb!" . toVerb
            parseVerb = (fmap snd . P.runParser LP.wordOnly) >=> toVerb'
        in swap (fmap parseVerb fwv) >>= flip conjugate st
