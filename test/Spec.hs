import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)

import Control.Applicative
import Data.Maybe (isJust)
import Linguistics.Conjugate
import Linguistics.FullWord (toVerb)
import Linguistics.Parsers (wordOnly)
import Linguistics.Render
import Linguistics.Types
import Linguistics.VerbEnding
import Parser

main :: IO ()
main =
    hspec $ do
        describe "conjugation and rendering" $ do
            describe "character change" $ do
                it "handles c -> qu from a -> e" $
                    tocar (PresentSubjunctive Nosotros) `shouldBe` "toquemos"
                it "handles c -> qu from a -> é" $
                    tocar (Preterite Yo) `shouldBe` "toqué"
                it "handles z -> c from a -> e" $
                    gozar (PresentSubjunctive Nosotros) `shouldBe` "gocemos"
                it "handles z -> c from a -> é" $
                    gozar (Preterite Yo) `shouldBe` "gocé"
                it "handles u -> ü from a -> e" $
                    averiguar (PresentSubjunctive Nosotros) `shouldBe`
                    "averigüemos"
                it "handles qu -> c from e -> a" $
                    delinquir (PresentSubjunctive Nosotros) `shouldBe`
                    "delincamos"
                it "handles c -> z from e -> o" $
                    vencer (Present Yo) `shouldBe` "venzo"
                it "handles g -> j from e -> a" $
                    proteger (PresentSubjunctive Nosotros) `shouldBe`
                    "protejamos"
                it "handles gu -> g from i -> a" $
                    distinguir (PresentSubjunctive Nosotros) `shouldBe`
                    "distingamos"
                it "handles +y in uir -> o" $
                    construir (Present Yo) `shouldBe` "construyo"
                it "handles +y and -ü from üir -> o" $
                    argüir (Present Yo) `shouldBe` "arguyo"
            describe "handles no special rules for..." $ do
                it "averiguar + amos" $
                    averiguar (Present Nosotros) `shouldBe` "averiguamos"
                it "hablar + emos" $
                    hablar (PresentSubjunctive Nosotros) `shouldBe` "hablemos"
                it "distinguir + es" $
                    distinguir (Present Tú) `shouldBe` "distingues"
                it "correr + amos" $
                    correr (PresentSubjunctive Nosotros) `shouldBe` "corramos"
                it "construir + ía" $
                    construir (Imperfect Usted) `shouldBe` "construía"
                it "argüir + ía" $ argüir (Imperfect Usted) `shouldBe` "argüía"
            describe "accent i when stressed" $ do
                it "0" $ caer (Preterite Nosotros) `shouldBe` "caímos"
                it "1" $ caer (Preterite Tú) `shouldBe` "caíste"
                it "2" $ leer (Preterite Nosotros) `shouldBe` "leímos"
                it "3" $ oír (Preterite Nosotros) `shouldBe` "oímos"
            describe "remove accents in monosyllabic forms" $ do
                it "0" $ liar (Preterite Yo) `shouldBe` "lie"
                it "1" $ liar (Preterite Usted) `shouldBe` "lio"
                it "2" $ ver (Preterite Yo) `shouldBe` "vi"
                it "3" $ ver (Preterite Usted) `shouldBe` "vio"
            describe "replace an semivowel left i with a y between cores" $ do
                it "0" $ caer (Preterite Usted) `shouldBe` "cayó"
                it "1" $ caer (Preterite Ustedes) `shouldBe` "cayeron"
                it "2" $ construir (Preterite Usted) `shouldBe` "construyó"
                it "3" $ argüir PresentParticiple `shouldBe` "arguyendo"
            describe
                "not replace an unstressed i with a y between a silent vowels" $ do
                it "0" $ delinquir (Preterite Usted) `shouldBe` "delinquió"
                it "1" $ delinquir (Preterite Ustedes) `shouldBe` "delinquieron"
                it "2" $ distinguir (Preterite Usted) `shouldBe` "distinguió"
                it "3" $
                    distinguir (Preterite Ustedes) `shouldBe` "distinguieron"
            describe "drop unneeded semi-vowel left" $ do
                it "drop a semivowel left i after a ll" $
                    bullir (Preterite Usted) `shouldBe` "bulló"
                it "drop a semivowel left i after a ñ" $
                    tañer (Preterite Usted) `shouldBe` "tañó"
            describe "should stem change if stressed" $ do
                it "e" $ pensar (Present Yo) `shouldBe` "pienso"
                it "o" $ contar (Present Yo) `shouldBe` "cuento"
                it "i" $ adquirir (Present Yo) `shouldBe` "adquiero"
                it "u" $ jugar (Present Yo) `shouldBe` "juego"
            describe "should not stem change if not stressed" $ do
                it "e" $ pensar (Present Nosotros) `shouldBe` "pensamos"
                it "o" $ contar (Present Nosotros) `shouldBe` "contamos"
                it "i" $ adquirir (Present Nosotros) `shouldBe` "adquirimos"
                it "u" $ jugar (Present Nosotros) `shouldBe` "jugamos"
            describe "should not stem change when ending is explicitly accented" $ do
                it "e" $ pensar (Preterite Usted) `shouldBe` "pensó"
                it "o" $ contar (Preterite Usted) `shouldBe` "contó"
                it "i" $ adquirir (Preterite Usted) `shouldBe` "adquirió"
                it "u" $ jugar (Preterite Usted) `shouldBe` "jugó"
            describe
                "raise for non-diphthongizers when the ending doesn't have ir or í" $ do
                it "o" $ pedir (Present Yo) `shouldBe` "pido"
                it "a" $ pedir (PresentSubjunctive Usted) `shouldBe` "pida"
                it "e" $ pedir (Present Usted) `shouldBe` "pide"
            describe
                "raise for diphthongizers when no diphthong is created and ending does not included ir or í" $ do
                it "durmamos" $
                    dormir (PresentSubjunctive Nosotros) `shouldBe` "durmamos"
                it "durmiendo" $ dormir PresentParticiple `shouldBe` "durmiendo"
                it "durmieron" $
                    dormir (Preterite Ustedes) `shouldBe` "durmieron"
                it "sintamos" $
                    sentir (PresentSubjunctive Nosotros) `shouldBe` "sintamos"
                it "sintiendo" $ sentir PresentParticiple `shouldBe` "sintiendo"
                it "sintieron" $
                    sentir (Preterite Ustedes) `shouldBe` "sintieron"
            describe
                "not raise for diphthongizers when a diphthong is created when it otherwise would" $ do
                it "duermo" $ dormir (Present Yo) `shouldBe` "duermo"
                it "duerma" $
                    dormir (PresentSubjunctive Usted) `shouldBe` "duerma"
                it "duerme" $ dormir (Present Usted) `shouldBe` "duerme"
                it "siento" $ sentir (Present Yo) `shouldBe` "siento"
                it "sienta" $
                    sentir (PresentSubjunctive Usted) `shouldBe` "sienta"
                it "siente" $ sentir (Present Usted) `shouldBe` "siente"
            describe "misc" $ do
                it "should not start with a semivowel i" $
                    oler (Present Yo) `shouldBe` "huelo"
                it "should not start with a semivowel u" $
                    errar (Present Yo) `shouldBe` "yerro"
                it "should preserve a hard g in 'go' -> 'gue'" $
                    avergonzar (Present Yo) `shouldBe` "avergüenzo"
                it
                    "not raise when the ending does not included ir or í (diphthong or not)" $
                    sentir PastParticiple `shouldBe` "sentido"
        describe "parser + render" $
            prop
                "should give back the same result if parsed then rendered"
                (\x ->
                     case runParser wordOnly x of
                         Right (s, "") -> render s == x
                         _ -> True)
        describe "getEnding" $
            it "does not throw for any ending" $
            -- There may be a more clever way to make sure this is exhaustive
            -- but notably, if a new tense is added, the type checker won't let us know
            let allTenses =
                    [Infinitive, PastParticiple, PresentParticiple] ++
                    ([ Conditional
                     , Future
                     , Imperfect
                     , Present
                     , Preterite
                     , PresentSubjunctive
                     , ImperfectSubjunctive
                     ] <*>
                     [Yo, Tú, Usted, Nosotros, Ustedes])
                allEndings = liftA2 getEnding [AR, ER, IR] allTenses
                -- Evaluation of the list must be force, done here by checking for a coda
            in length (filter (\(_, _, x) -> isJust x) allEndings) `shouldBe` 63

type VerbHelper = SimpleTense -> String

-- TODO: these are probably _close_ to being useful lib functions
cong :: Bool -> Bool -> FullWord -> SimpleTense -> Maybe String
cong diph vR fw st = fmap render (toVerb fw >>= flip (conjugate diph vR) st)

-- This is not total, but that's actually perfect for our tests
v :: Bool -> Bool -> String -> VerbHelper
v diph vR str st =
    let renderedParser = fmap (\fw -> cong diph vR fw st) wordOnly
    in case runParser renderedParser str of
           Right (Just rendered, "") -> rendered
           Right (Just _, _:_)
               -- This case really should be impossible by using `wordOnly`
            ->
               error
                   "Test inputs are invalid!  Verb string was not fully parsed!"
           Right (Nothing, _) ->
               error
                   "Test inputs are invalid!  Verb had no valid conjugation (did not end with ar/er/ir, could not diphthongize, etc)!"
           Left _ ->
               error "Test inputs are invalid!  Verb string was not parseable!"

tocar :: VerbHelper
tocar = v False False "tocar"

gozar :: VerbHelper
gozar = v False False "gozar"

averiguar :: VerbHelper
averiguar = v False False "averiguar"

delinquir :: VerbHelper
delinquir = v False False "delinquir"

vencer :: VerbHelper
vencer = v False False "vencer"

proteger :: VerbHelper
proteger = v False False "proteger"

distinguir :: VerbHelper
distinguir = v False False "distinguir"

construir :: VerbHelper
construir = v False False "construir"

argüir :: VerbHelper
argüir = v False False "argüir"

hablar :: VerbHelper
hablar = v False False "hablar"

correr :: VerbHelper
correr = v False False "correr"

caer :: VerbHelper
caer = v False False "caer"

leer :: VerbHelper
leer = v False False "leer"

oír :: VerbHelper
oír = v False False "oír"

liar :: VerbHelper
liar = v False False "liar"

ver :: VerbHelper
ver = v False False "ver"

bullir :: VerbHelper
bullir = v False False "bullir"

tañer :: VerbHelper
tañer = v False False "tañer"

pensar :: VerbHelper
pensar = v True False "pensar"

contar :: VerbHelper
contar = v True False "contar"

adquirir :: VerbHelper
adquirir = v True False "adquirir"

jugar :: VerbHelper
jugar = v True False "jugar"

oler :: VerbHelper
oler = v True False "oler"

errar :: VerbHelper
errar = v True False "errar"

avergonzar :: VerbHelper
avergonzar = v True False "avergonzar"

pedir :: VerbHelper
pedir = v False True "pedir"

dormir :: VerbHelper
dormir = v True True "dormir"

sentir :: VerbHelper
sentir = v True True "sentir"