{-# LANGUAGE FlexibleInstances #-}

module Linguistics.VerbEnding
    ( getEnding
    , HasVerbEnding
    ) where

import Linguistics.Parsers (endingOnly)
import Linguistics.Types
import Parser

class Show a =>
      HasVerbEnding a where
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
            Right (z, _) -> z
            _ ->
                error
                    ("An ending was unparsable! For " ++
                     show vt ++ " - " ++ show a ++ ".")
    getEnding' :: VerbType -> a -> String

instance HasVerbEnding (SubjectSensativeTense, Subject) where
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

instance HasVerbEnding SubjectlessTense where
    getEnding' AR Infinitive = "ar"
    getEnding' AR PastParticiple = "ado"
    getEnding' AR PresentParticiple = "ando"
    getEnding' ER Infinitive = "er"
    getEnding' IR Infinitive = "ir"
    getEnding' _ PastParticiple = "ido"
    getEnding' _ PresentParticiple = "iendo"
