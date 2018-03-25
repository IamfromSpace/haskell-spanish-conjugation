module Linguistics.VerbEnding
    ( getEnding
    ) where

import Linguistics.Parsers
import Linguistics.Types
import Parser

getEnding' :: VerbType -> SimpleTense -> String
getEnding' AR Infinitive = "ar"
getEnding' AR PastParticiple = "ado"
getEnding' AR PresentParticiple = "ando"
getEnding' AR (Conditional Tú) = "arías"
getEnding' AR (Conditional Nosotros) = "aríamos"
getEnding' AR (Conditional Ustedes) = "arían"
getEnding' AR (Conditional _) = "aría"
getEnding' AR (Future Yo) = "aré"
getEnding' AR (Future Tú) = "arás"
getEnding' AR (Future Usted) = "ará"
getEnding' AR (Future Nosotros) = "aremos"
getEnding' AR (Future Ustedes) = "arán"
getEnding' AR (Imperfect Tú) = "abas"
getEnding' AR (Imperfect Nosotros) = "ábamos"
getEnding' AR (Imperfect Ustedes) = "aban"
getEnding' AR (Imperfect _) = "aba"
getEnding' AR (Present Tú) = "as"
getEnding' AR (Present Usted) = "a"
getEnding' AR (Present Nosotros) = "amos"
getEnding' AR (Present Ustedes) = "an"
getEnding' AR (Preterite Yo) = "é"
getEnding' AR (Preterite Tú) = "aste"
getEnding' AR (Preterite Usted) = "ó"
getEnding' AR (Preterite Nosotros) = getEnding' AR (Present Nosotros)
getEnding' AR (Preterite Ustedes) = "aron"
getEnding' AR (PresentSubjunctive Yo) = getEnding' ER (Present Usted)
getEnding' AR (PresentSubjunctive x) = getEnding' ER (Present x)
getEnding' AR (ImperfectSubjunctive Tú) = "aras"
getEnding' AR (ImperfectSubjunctive Nosotros) = "áramos"
getEnding' AR (ImperfectSubjunctive Ustedes) = "aran"
getEnding' AR (ImperfectSubjunctive _) = "ara"
getEnding' ER Infinitive = "er"
getEnding' ER (Conditional Tú) = "erías"
getEnding' ER (Conditional Nosotros) = "eríamos"
getEnding' ER (Conditional Ustedes) = "erían"
getEnding' ER (Conditional _) = "ería"
getEnding' ER (Future Yo) = "eré"
getEnding' ER (Future Tú) = "erás"
getEnding' ER (Future Usted) = "erá"
getEnding' ER (Future Nosotros) = "eremos"
getEnding' ER (Future Ustedes) = "erán"
getEnding' ER (Present Nosotros) = "emos"
getEnding' IR Infinitive = "ir"
getEnding' IR (Conditional Tú) = "irías"
getEnding' IR (Conditional Nosotros) = "iríamos"
getEnding' IR (Conditional Ustedes) = "irían"
getEnding' IR (Conditional _) = "iría"
getEnding' IR (Future Yo) = "iré"
getEnding' IR (Future Tú) = "irás"
getEnding' IR (Future Usted) = "irá"
getEnding' IR (Future Nosotros) = "iremos"
getEnding' IR (Future Ustedes) = "irán"
getEnding' IR (Present Nosotros) = "imos"
getEnding' _ PastParticiple = "ido"
getEnding' _ PresentParticiple = "iendo"
getEnding' _ (Imperfect Tú) = "ías"
getEnding' _ (Imperfect Nosotros) = "íamos"
getEnding' _ (Imperfect Ustedes) = "ían"
getEnding' _ (Imperfect _) = "ía"
getEnding' _ (Present Yo) = "o"
getEnding' _ (Present Tú) = "es"
getEnding' _ (Present Usted) = "e"
getEnding' _ (Present Ustedes) = "en"
getEnding' _ (Preterite Yo) = "í"
getEnding' _ (Preterite Tú) = "iste"
getEnding' _ (Preterite Usted) = "ió"
getEnding' _ (Preterite Nosotros) = getEnding' IR (Present Nosotros)
getEnding' _ (Preterite Ustedes) = "ieron"
getEnding' _ (PresentSubjunctive Yo) = getEnding' AR (Present Usted)
getEnding' _ (PresentSubjunctive x) = getEnding' AR (Present x)
getEnding' _ (ImperfectSubjunctive Tú) = "ieras"
getEnding' _ (ImperfectSubjunctive Nosotros) = "iéramos"
getEnding' _ (ImperfectSubjunctive Ustedes) = "ieran"
getEnding' _ (ImperfectSubjunctive _) = "iera"

-- This is not a total function, but since getEnding' can only
-- return valid parseable endings, a failed parse here is
-- completely exceptional, and should throw.
-- Automated tests validate that this does not throw in any case.
-- An alternative here that would be total/typechecked/efficient is
-- to simply return each ending directly, however, these endings are
-- just a mess and are basically meaningless to the human reader.
-- It just doesn't seem like a practical solution.
getEnding :: VerbType -> SimpleTense -> Ending
getEnding x y =
    let Right (z, _) = runParser endingOnly (getEnding' x y)
    in z
