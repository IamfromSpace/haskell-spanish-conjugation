{-# LANGUAGE FlexibleInstances #-}

module Linguistics.Conjugate
    ( conjugate
    , CanConjugate
    , Tense
    , isPastParticiple
    , getEnding
    , getIrregularPreteriteEffect
    , handleHopeless
    ) where

import Control.Lens
import Control.Monad ((>=>), join)
import Data.Maybe (fromMaybe)
import Linguistics.Diphthongizing
import Linguistics.FullWord
import Linguistics.Intermediate
import Linguistics.Parsers (endingOnly, wordOnly)
import qualified Linguistics.Parsers as LP
import Linguistics.Types
import Linguistics.VowelRaising
import Parser
import qualified Parser as P
import Utils (($>), ifAppA, maybeUnit, swap, withLeft)

-- Has to be restricted to a string?
class CanConjugate a where
    conjugate :: Tense b => a -> b -> Either String FullWord

instance CanConjugate (VerbConfig FullWord InnerSyllable', Verb) where
    conjugate ((irregularPastParticiple, irregularPreterite, hasIrregularInfinitives, isYoGoVerb, isZcVerb, isDiphthongBreaking, isDiphthongizing, isVowelRaising), verb@(vt, _, _, _)) tense =
        case maybeUnit (isPastParticiple tense) *> irregularPastParticiple of
            Just x -> return x
            Nothing ->
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
                        preventUirNonIDiphthongization
                            (toIntermediate verb ending)
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

instance CanConjugate (VerbConfig FullWord InnerSyllable', String) where
    conjugate fwv st =
        let toVerb' = withLeft "word is not a verb!" . toVerb
            parseVerb = (fmap snd . P.runParser LP.wordOnly) >=> toVerb'
        in swap (fmap parseVerb fwv) >>= flip conjugate st

instance CanConjugate (VerbConfig FullWord String, String) where
    conjugate fwv st =
        let parse :: String -> Either String InnerSyllable'
            parse = fmap snd . P.runParser LP.onlyInnerSyllable'
            getParsedIrregularPreteriteData
             -- Grab the irregular preterite data with view,
             -- then fmap in, parse, and then swap the Either to the top
             = swap . fmap (swap . fmap parse) . view (_1 . _2)
            withParsedConfig
             -- update the irregular preterite, with the whole resulting
             -- config inside the Either from the parse.
             = flip (set (_1 . _2)) fwv <$> getParsedIrregularPreteriteData fwv
        in withParsedConfig >>= flip conjugate st

instance CanConjugate (VerbConfig String String, String) where
    conjugate fwv st =
        let parse :: String -> Either String FullWord
            parse = fmap snd . P.runParser LP.wordOnly
            withParsedConfig =
                fmap
                    (flip (set (_1 . _1)) fwv)
                    (swap (fmap parse (view (_1 . _1) fwv)))
        in withParsedConfig >>= flip conjugate st

instance CanConjugate HopelessVerb where
    conjugate = handleHopeless

makeSimpleEnding :: LowVowel -> Ending
makeSimpleEnding lowVowel =
    ((False, (Nothing, Right (lowVowel, Nothing))), [], Nothing)

dangerousRepresentation :: HopelessVerb -> (VerbConfig String String, String)
dangerousRepresentation v =
    case v of
        Dar ->
            ( (Nothing, Nothing, False, False, False, False, False, False)
            , "dar")
        Estar ->
            ( ( Nothing
              , Just (False, "uv")
              , False
              , False
              , False
              , False
              , False
              , False)
            , "estar")
        Haber ->
            ( ( Nothing
              , Just (True, "ub")
              , True
              , False
              , False
              , False
              , False
              , False)
            , "haber")
        Ir ->
            ((Nothing, Nothing, False, False, False, False, False, False), "ir")
        Prever ->
            ( ( Just "previsto"
              , Nothing
              , False
              , False
              , False
              , False
              , False
              , False)
            , "prever")
        Saber ->
            ( ( Nothing
              , Just (True, "up")
              , True
              , False
              , False
              , False
              , False
              , False)
            , "saber")
        Ser ->
            ( (Nothing, Nothing, False, False, False, False, False, False)
            , "ser")
        Ver ->
            ( (Just "visto", Nothing, False, False, False, False, False, False)
            , "ver")

class Show a =>
      Tense a where
    isPastParticiple :: a -> Bool
    getIrregularPreteriteEffect :: a -> IrregularPreteriteEffect
    getEnding :: VerbType -> a -> Ending
    -- This is not a total function, but since getEnding' can only
    -- return valid parseable endings, a failed parse here is
    -- completely exceptional, and should throw.
    -- Automated tests validate that this does not throw in any case.
    -- An alternative here that would be total/typechecked/efficient is
    -- to simply return each ending directly, however, these endings are
    -- just a mess and are basically meaningless to the human reader.
    -- It just doesn't seem like a practical solution.
    handleHopeless :: HopelessVerb -> a -> Either String FullWord
    getEnding vt a =
        case runParser endingOnly (getEnding' vt a) of
            Right (_, z) -> z
            _ ->
                error
                    ("An ending was unparsable! For " ++
                     show vt ++ " - " ++ show a ++ ".")
    getEnding' :: VerbType -> a -> String

fwParse :: String -> Either String FullWord
fwParse = fmap snd . runParser wordOnly

dangerousDarWithEr :: (VerbConfig String String, String)
dangerousDarWithEr =
    ((Nothing, Nothing, False, False, False, False, False, False), "der")

dangerousHaberAsHayer :: (VerbConfig String String, String)
dangerousHaberAsHayer =
    ((Nothing, Nothing, False, False, False, False, False, False), "hayer")

dangerousIrAsVar :: (VerbConfig String String, String)
dangerousIrAsVar =
    ((Nothing, Nothing, False, False, False, False, False, False), "var")

dangerousIrAsVayer :: (VerbConfig String String, String)
dangerousIrAsVayer =
    ((Nothing, Nothing, False, False, False, False, False, False), "vayer")

dangerousSaberAsSeper :: (VerbConfig String String, String)
dangerousSaberAsSeper =
    ((Nothing, Nothing, False, False, False, False, False, False), "seper")

dangerousSerAsSeer :: (VerbConfig String String, String)
dangerousSerAsSeer =
    ((Nothing, Nothing, False, False, False, False, False, False), "seer")

dangerousVerAsVeer :: (VerbConfig String String, String)
dangerousVerAsVeer =
    ((Nothing, Nothing, False, False, False, False, False, False), "veer")

dangerousPreverAsPreveer :: (VerbConfig String String, String)
dangerousPreverAsPreveer =
    ((Nothing, Nothing, False, False, False, False, False, False), "preveer")

--Ideally Tense would be its own module, but that causes a circular imports
instance Tense (SubjectSensativeTense, Subject) where
    isPastParticiple _ = False
    --
    getIrregularPreteriteEffect (Preterite, Yo) =
        Just (Just (makeSimpleEnding E))
    getIrregularPreteriteEffect (Preterite, Usted) =
        Just (Just (makeSimpleEnding O))
    getIrregularPreteriteEffect (Preterite, Él) = Just Nothing
    getIrregularPreteriteEffect (Preterite, _) = Just Nothing
    getIrregularPreteriteEffect (ImperfectSubjunctive, _) = Just Nothing
    getIrregularPreteriteEffect _ = Nothing
    --
    handleHopeless x (y, Él) = handleHopeless x (y, Usted)
    handleHopeless x (y, Ellos) = handleHopeless x (y, Ustedes)
    handleHopeless Dar (Present, Yo) = fwParse "doy"
    handleHopeless Dar (PresentSubjunctive, Yo) = fwParse "dé"
    handleHopeless Dar (PresentSubjunctive, Usted) = fwParse "dé"
    handleHopeless Dar x@(Preterite, _) = conjugate dangerousDarWithEr x
    handleHopeless Dar x@(ImperfectSubjunctive, _) =
        conjugate dangerousDarWithEr x
    handleHopeless Estar (Present, Yo) = fwParse "estoy"
    handleHopeless Estar (Present, Tú) = fwParse "estás"
    handleHopeless Estar (Present, Usted) = fwParse "está"
    handleHopeless Estar (Present, Ustedes) = fwParse "están"
    handleHopeless Estar (PresentSubjunctive, Yo) = fwParse "esté"
    handleHopeless Estar (PresentSubjunctive, Tú) = fwParse "estés"
    handleHopeless Estar (PresentSubjunctive, Usted) = fwParse "esté"
    handleHopeless Estar (PresentSubjunctive, Ustedes) = fwParse "estén"
    handleHopeless Haber (Present, Yo) = fwParse "he"
    handleHopeless Haber (Present, Tú) = fwParse "has"
    handleHopeless Haber (Present, Usted) = fwParse "ha"
    handleHopeless Haber (Present, Nosotros) = fwParse "hemos"
    handleHopeless Haber (Present, Ustedes) = fwParse "han"
    handleHopeless Haber x@(PresentSubjunctive, _) =
        conjugate dangerousHaberAsHayer x
    handleHopeless Ir (Present, Yo) = fwParse "voy"
    handleHopeless Ir x@(Present, _) = conjugate dangerousIrAsVar x
    handleHopeless Ir x@(PresentSubjunctive, _) = conjugate dangerousIrAsVayer x
    handleHopeless Ir (Imperfect, Yo) = fwParse "iba"
    handleHopeless Ir (Imperfect, Tú) = fwParse "ibas"
    handleHopeless Ir (Imperfect, Usted) = fwParse "iba"
    handleHopeless Ir (Imperfect, Nosotros) = fwParse "ibamos"
    handleHopeless Ir (Imperfect, Ustedes) = fwParse "iban"
    handleHopeless Ir (Preterite, Yo) = fwParse "fui"
    handleHopeless Ir (Preterite, Tú) = fwParse "fuiste"
    handleHopeless Ir (Preterite, Usted) = fwParse "fue"
    handleHopeless Ir (Preterite, Nosotros) = fwParse "fuimos"
    handleHopeless Ir (Preterite, Ustedes) = fwParse "fueron"
    handleHopeless Ir (ImperfectSubjunctive, Yo) = fwParse "fuera"
    handleHopeless Ir (ImperfectSubjunctive, Tú) = fwParse "fueras"
    handleHopeless Ir (ImperfectSubjunctive, Usted) = fwParse "fuera"
    handleHopeless Ir (ImperfectSubjunctive, Nosotros) = fwParse "fuéramos"
    handleHopeless Ir (ImperfectSubjunctive, Ustedes) = fwParse "fueran"
    handleHopeless Saber (Present, Yo) = fwParse "sé"
    handleHopeless Saber x@(PresentSubjunctive, _) =
        conjugate dangerousSaberAsSeper x
    handleHopeless Ser x@(PresentSubjunctive, _) =
        conjugate dangerousSerAsSeer x
    handleHopeless Ser (Present, Yo) = fwParse "soy"
    handleHopeless Ser (Present, Tú) = fwParse "eres"
    handleHopeless Ser (Present, Usted) = fwParse "es"
    handleHopeless Ser (Present, Nosotros) = fwParse "somos"
    handleHopeless Ser (Present, Ustedes) = fwParse "son"
    handleHopeless Ser (Imperfect, Yo) = fwParse "era"
    handleHopeless Ser (Imperfect, Tú) = fwParse "eras"
    handleHopeless Ser (Imperfect, Usted) = fwParse "era"
    handleHopeless Ser (Imperfect, Nosotros) = fwParse "éramos"
    handleHopeless Ser (Imperfect, Ustedes) = fwParse "eran"
    handleHopeless Ser x@(Preterite, _) = handleHopeless Ir x
    handleHopeless Ser x@(ImperfectSubjunctive, _) = handleHopeless Ir x
    handleHopeless Ver (Present, Yo) = fwParse "veo"
    handleHopeless Ver x@(Imperfect, _) = conjugate dangerousVerAsVeer x
    handleHopeless Ver x@(PresentSubjunctive, _) =
        conjugate dangerousVerAsVeer x
    handleHopeless Prever (Present, Yo) = fwParse "preveo"
    handleHopeless Prever (Present, Tú) = fwParse "prevés"
    handleHopeless Prever (Present, Usted) = fwParse "prevé"
    handleHopeless Prever (Present, Nosotros) = fwParse "prevemos"
    handleHopeless Prever (Present, Ustedes) = fwParse "prevén"
    handleHopeless Prever x@(Imperfect, _) =
        conjugate dangerousPreverAsPreveer x
    handleHopeless Prever x@(PresentSubjunctive, _) =
        conjugate dangerousPreverAsPreveer x
    handleHopeless x y = conjugate (dangerousRepresentation x) y
    --
    getEnding' x (y, Él) = getEnding' x (y, Usted)
    getEnding' x (y, Ellos) = getEnding' x (y, Ustedes)
    getEnding' AR (Imperfect, Tú) = "abas"
    getEnding' AR (Imperfect, Nosotros) = "ábamos"
    getEnding' AR (Imperfect, Ustedes) = "aban"
    getEnding' AR (Imperfect, _) = "aba"
    getEnding' AR (Present, Tú) = "as"
    getEnding' AR (Present, Usted) = "a"
    getEnding' AR (Present, Nosotros) = "amos"
    getEnding' AR (Present, Ustedes) = "an"
    getEnding' AR (Preterite, Yo) = "é"
    getEnding' AR (Preterite, Tú) = "aste"
    getEnding' AR (Preterite, Usted) = "ó"
    getEnding' AR (Preterite, Nosotros) = getEnding' AR (Present, Nosotros)
    getEnding' AR (Preterite, Ustedes) = "aron"
    getEnding' AR (PresentSubjunctive, Yo) = getEnding' ER (Present, Usted)
    getEnding' AR (PresentSubjunctive, x) = getEnding' ER (Present, x)
    getEnding' AR (ImperfectSubjunctive, Tú) = "aras"
    getEnding' AR (ImperfectSubjunctive, Nosotros) = "áramos"
    getEnding' AR (ImperfectSubjunctive, Ustedes) = "aran"
    getEnding' AR (ImperfectSubjunctive, _) = "ara"
    getEnding' ER (Present, Nosotros) = "emos"
    getEnding' IR (Present, Nosotros) = "imos"
    getEnding' vt (Conditional, Tú) = getEnding' vt Infinitive ++ "ías"
    getEnding' vt (Conditional, Nosotros) = getEnding' vt Infinitive ++ "íamos"
    getEnding' vt (Conditional, Ustedes) = getEnding' vt Infinitive ++ "ían"
    getEnding' vt (Conditional, _) = getEnding' vt Infinitive ++ "ía"
    getEnding' vt (Future, Yo) = getEnding' vt Infinitive ++ "é"
    getEnding' vt (Future, Tú) = getEnding' vt Infinitive ++ "ás"
    getEnding' vt (Future, Usted) = getEnding' vt Infinitive ++ "á"
    getEnding' vt (Future, Nosotros) = getEnding' vt Infinitive ++ "emos"
    getEnding' vt (Future, Ustedes) = getEnding' vt Infinitive ++ "án"
    getEnding' _ (Imperfect, Tú) = "ías"
    getEnding' _ (Imperfect, Nosotros) = "íamos"
    getEnding' _ (Imperfect, Ustedes) = "ían"
    getEnding' _ (Imperfect, _) = "ía"
    getEnding' _ (Present, Yo) = "o"
    getEnding' _ (Present, Tú) = "es"
    getEnding' _ (Present, Usted) = "e"
    getEnding' _ (Present, Ustedes) = "en"
    getEnding' _ (Preterite, Yo) = "í"
    getEnding' _ (Preterite, Tú) = "iste"
    getEnding' _ (Preterite, Usted) = "ió"
    getEnding' _ (Preterite, Nosotros) = getEnding' IR (Present, Nosotros)
    getEnding' _ (Preterite, Ustedes) = "ieron"
    getEnding' _ (PresentSubjunctive, Yo) = getEnding' AR (Present, Usted)
    getEnding' _ (PresentSubjunctive, x) = getEnding' AR (Present, x)
    getEnding' _ (ImperfectSubjunctive, Tú) = "ieras"
    getEnding' _ (ImperfectSubjunctive, Nosotros) = "iéramos"
    getEnding' _ (ImperfectSubjunctive, Ustedes) = "ieran"
    getEnding' _ (ImperfectSubjunctive, _) = "iera"

instance Tense SubjectlessTense where
    isPastParticiple PastParticiple = True
    isPastParticiple _ = False
    --
    getIrregularPreteriteEffect _ = Nothing
    --
    handleHopeless x = conjugate (dangerousRepresentation x)
    --
    getEnding' AR Infinitive = "ar"
    getEnding' AR PastParticiple = "ado"
    getEnding' AR PresentParticiple = "ando"
    getEnding' ER Infinitive = "er"
    getEnding' IR Infinitive = "ir"
    getEnding' _ PastParticiple = "ido"
    getEnding' _ PresentParticiple = "iendo"
