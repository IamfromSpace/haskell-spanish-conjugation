{-# LANGUAGE FlexibleInstances #-}

module Linguistics.Conjugate
    ( conjugate
    , CanConjugate
    ) where

import Control.Lens
import Control.Monad ((>=>), join)
import Data.Maybe (fromMaybe)
import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.Intermediate
import qualified Linguistics.Parsers as LP
import Linguistics.PossibleIrregularPreteriteEffect
import Linguistics.Types
import Linguistics.VerbEnding
import Linguistics.VowelRaising
import qualified Parser as P
import Utils (($>), ifAppA, swap, withLeft)

-- Has to be restricted to a string?
class CanConjugate a where
    conjugate ::
           (HasVerbEnding b, PossibleIrregularPreteriteEffect b)
        => a
        -> b
        -> Either String FullWord

instance CanConjugate (VerbConfig InnerSyllable', Verb) where
    conjugate ((irregularPreterite, hasIrregularInfinitives, isYoGoVerb, isZcVerb, isDiphthongBreaking, isDiphthongizing, isVowelRaising), verb@(vt, _, _, _)) tense =
        let irregularPreteriteEffect =
                irregularPreterite *> getIrregularPreteriteEffect tense
            -- all verbs with an irregular preterite in an affected tense
            -- use ER endings (ER/IR are the same in affected tenses)
            verbType = fromMaybe vt (irregularPreteriteEffect $> ER)
            -- we use the expected ending, unless there are irregular preterite effects
            ending =
                getEnding verbType tense `fromMaybe`
                join irregularPreteriteEffect
            intermediate =
                preventUirNonIDiphthongization (toIntermediate verb ending)
            willYoGo = isYoGoVerb && couldYoGo intermediate
            willDiphthongize =
                isDiphthongizing &&
                couldDiphthongize intermediate && not willYoGo
            willVowelRaise =
                isVowelRaising &&
                couldVowelRaise intermediate && not willDiphthongize
            mIntermediate
                -- Chain rules that could fail and may or may not apply
             =
                ifAppA breakDiphthong isDiphthongBreaking intermediate >>=
                ifAppA diphthongize willDiphthongize >>=
                ifAppA raiseVowel willVowelRaise >>=
                ifAppA cToZc (isZcVerb && couldCToZc intermediate) >>=
                ifAppA yoGo willYoGo >>=
                ifAppA
                    shortenedInfinitives
                    (hasIrregularInfinitives &&
                     couldShortenedInfinitives intermediate)
            mIntermediate' =
                case irregularPreterite <* irregularPreteriteEffect of
                    Just (shouldReplace, syllable) ->
                        mIntermediate >>=
                        updatePenultimateSyllable shouldReplace syllable
                    Nothing -> mIntermediate
        in withLeft "could not conjugate" $
           fmap
               (dropSemiVowelIAfterÑ .
                dropSemiVowelIAfterLl .
                dropMonosyllabicAccent .
                preventStartingSemiVowel .
                fromIntermediate .
                preventStressedJointDiphthongization .
                preventAmbiguiousJointDiphthongization)
               mIntermediate'

instance CanConjugate (VerbConfig InnerSyllable', String) where
    conjugate fwv st =
        let toVerb' = withLeft "word is not a verb!" . toVerb
            parseVerb = (fmap snd . P.runParser LP.wordOnly) >=> toVerb'
        in swap (fmap parseVerb fwv) >>= flip conjugate st

instance CanConjugate (VerbConfig String, String) where
    conjugate fwv st =
        let parse :: String -> Either String InnerSyllable'
            parse = fmap snd . P.runParser LP.onlyInnerSyllable'
            getParsedIrregularPreteriteData
             -- Grab the irregular preterite data with view,
             -- then fmap in, parse, and then swap the Either to the top
             = swap . fmap (swap . fmap parse) . view (_1 . _1)
            withParsedConfig
             -- update the irregular preterite, with the whole resulting
             -- config inside the Either from the parse.
             = flip (set (_1 . _1)) fwv <$> getParsedIrregularPreteriteData fwv
        in withParsedConfig >>= flip conjugate st
