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
conjugate ::
       HasVerbEnding a
    => (Bool, Bool, Bool, Bool, Bool)
    -> Verb
    -> a
    -> Maybe FullWord
conjugate (isYoGoVerb, isZcVerb, isDiphthongBreaking, isDiphthongizing, isVowelRaising) verb@(vt, _, _, _) tense =
    let ending = getEnding vt tense
        intermediate =
            preventUirNonIDiphthongization (toIntermediate verb ending)
        willYoGo = isYoGoVerb && couldYoGo intermediate
        willDiphthongize =
            isDiphthongizing && couldDiphthongize intermediate && not willYoGo
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
            fmap
                (preventStressedJointDiphthongization .
                 preventAmbiguiousJointDiphthongization)
                mIntermediate''''
        fullWord =
            fmap
                (dropSemiVowelIAfterÑ .
                 dropSemiVowelIAfterLl .
                 dropMonosyllabicAccent .
                 preventStartingSemiVowel . fromIntermediate)
                mIntermediate'''''
    in fullWord
