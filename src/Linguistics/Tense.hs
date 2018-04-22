{-# LANGUAGE FlexibleInstances #-}

module Linguistics.Tense
    ( Tense
    , isPastParticiple
    , getEnding
    , getIrregularPreteriteEffect
    ) where

import Linguistics.Parsers (endingOnly)
import Linguistics.Types
import Parser

makeSimpleEnding :: LowVowel -> Ending
makeSimpleEnding lowVowel =
    ((False, (Nothing, Right (lowVowel, Nothing))), [], Nothing)

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
    getEnding vt a =
        case runParser endingOnly (getEnding' vt a) of
            Right (_, z) -> z
            _ ->
                error
                    ("An ending was unparsable! For " ++
                     show vt ++ " - " ++ show a ++ ".")
    getEnding' :: VerbType -> a -> String

instance Tense (SubjectSensativeTense, Subject) where
    isPastParticiple _ = False
    getIrregularPreteriteEffect (Preterite, Yo) =
        Just (Just (makeSimpleEnding E))
    getIrregularPreteriteEffect (Preterite, Usted) =
        Just (Just (makeSimpleEnding O))
    getIrregularPreteriteEffect (Preterite, Él) = Just Nothing
    getIrregularPreteriteEffect (Preterite, _) = Just Nothing
    getIrregularPreteriteEffect (ImperfectSubjunctive, _) = Just Nothing
    getIrregularPreteriteEffect _ = Nothing
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
    getIrregularPreteriteEffect _ = Nothing
    getEnding' AR Infinitive = "ar"
    getEnding' AR PastParticiple = "ado"
    getEnding' AR PresentParticiple = "ando"
    getEnding' ER Infinitive = "er"
    getEnding' IR Infinitive = "ir"
    getEnding' _ PastParticiple = "ido"
    getEnding' _ PresentParticiple = "iendo"