import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)

import Control.Applicative
import Data.Maybe (isJust)
import Linguistics.Conjugate
import Linguistics.Parsers (endingOnly, wordOnly)
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
                    tocar "emos" `shouldBe` Right (Just "toquemos")
                it "handles c -> qu from a -> é" $
                    tocar "é" `shouldBe` Right (Just "toqué")
                it "handles z -> c from a -> e" $
                    gozar "emos" `shouldBe` Right (Just "gocemos")
                it "handles z -> c from a -> é" $
                    gozar "é" `shouldBe` Right (Just "gocé")
                it "handles u -> ü from a -> e" $
                    averiguar "emos" `shouldBe` Right (Just "averigüemos")
                it "handles qu -> c from e -> a" $
                    delinquir "amos" `shouldBe` Right (Just "delincamos")
                it "handles c -> z from e -> o" $
                    vencer "o" `shouldBe` Right (Just "venzo")
                it "handles g -> j from e -> a" $
                    proteger "amos" `shouldBe` Right (Just "protejamos")
                it "handles gu -> g from i -> a" $
                    distinguir "amos" `shouldBe` Right (Just "distingamos")
                it "handles +y in uir -> o" $
                    construir "o" `shouldBe` Right (Just "construyo")
                it "handles +y and -ü from üir -> o" $
                    argüir "o" `shouldBe` Right (Just "arguyo")
            describe "handles no special rules for..." $ do
                it "averiguar + amos" $
                    averiguar "amos" `shouldBe` Right (Just "averiguamos")
                it "hablar + emos" $
                    hablar "emos" `shouldBe` Right (Just "hablemos")
                it "hablar + emos" $
                    hablar "emos" `shouldBe` Right (Just "hablemos")
                it "distinguir + emos" $
                    distinguir "emos" `shouldBe` Right (Just "distinguemos")
                it "correr + amos" $
                    correr "amos" `shouldBe` Right (Just "corramos")
                it "construir + ía" $
                    construir "ía" `shouldBe` Right (Just "construía")
                it "argüir + ía" $ argüir "ía" `shouldBe` Right (Just "argüía")
            describe "accent i when stressed" $ do
                it "0" $ caer "imos" `shouldBe` Right (Just "caímos")
                it "1" $ caer "iste" `shouldBe` Right (Just "caíste")
                it "2" $ leer "imos" `shouldBe` Right (Just "leímos")
                it "3" $ oír "imos" `shouldBe` Right (Just "oímos")
            describe "remove accents in monosyllabic forms" $ do
                it "0" $ liar "é" `shouldBe` Right (Just "lie")
                it "1" $ liar "ó" `shouldBe` Right (Just "lio")
                it "2" $ ver "í" `shouldBe` Right (Just "vi")
                it "3" $ ver "ió" `shouldBe` Right (Just "vio")
            describe "replace an semivowel left i with a y between cores" $ do
                it "0" $ caer "ió" `shouldBe` Right (Just "cayó")
                it "1" $ caer "ieron" `shouldBe` Right (Just "cayeron")
                it "2" $ construir "ió" `shouldBe` Right (Just "construyó")
                it "3" $ argüir "iendo" `shouldBe` Right (Just "arguyendo")
            describe
                "not replace an unstressed i with a y between a silent vowels" $ do
                it "0" $ delinquir "ió" `shouldBe` Right (Just "delinquió")
                it "1" $
                    delinquir "ieron" `shouldBe` Right (Just "delinquieron")
                it "2" $ distinguir "ió" `shouldBe` Right (Just "distinguió")
                it "3" $
                    distinguir "ieron" `shouldBe` Right (Just "distinguieron")
            describe "drop unneeded semi-vowel left" $ do
                it "drop a semivowel left i after a ll" $
                    bullir "ió" `shouldBe` Right (Just "bulló")
                it "drop a semivowel left i after a ñ" $
                    tañer "ió" `shouldBe` Right (Just "tañó")
            describe "should stem change if stressed" $ do
                it "e" $ pensar "o" `shouldBe` Right (Just "pienso")
                it "o" $ contar "o" `shouldBe` Right (Just "cuento")
                it "i" $ adquirir "o" `shouldBe` Right (Just "adquiero")
                it "u" $ jugar "o" `shouldBe` Right (Just "juego")
            describe "should not stem change if not stressed" $ do
                it "e" $ pensar "amos" `shouldBe` Right (Just "pensamos")
                it "o" $ contar "amos" `shouldBe` Right (Just "contamos")
                it "i" $ adquirir "emos" `shouldBe` Right (Just "adquiremos")
                it "u" $ jugar "amos" `shouldBe` Right (Just "jugamos")
            describe "should not stem change when ending is explicitly accented" $ do
                it "e" $ pensar "ó" `shouldBe` Right (Just "pensó")
                it "o" $ contar "ó" `shouldBe` Right (Just "contó")
                it "i" $ adquirir "ió" `shouldBe` Right (Just "adquirió")
                it "u" $ jugar "ó" `shouldBe` Right (Just "jugó")
            describe
                "raise for non-diphthongizers when the ending doesn't have ir or í" $ do
                it "o" $ pedir "o" `shouldBe` Right (Just "pido")
                it "a" $ pedir "a" `shouldBe` Right (Just "pida")
                it "e" $ pedir "e" `shouldBe` Right (Just "pide")
            describe
                "raise for diphthongizers when no diphthong is created and ending does not included ir or í" $ do
                it "durmamos" $ dormir "amos" `shouldBe` Right (Just "durmamos")
                it "durmiendo" $
                    dormir "iendo" `shouldBe` Right (Just "durmiendo")
                it "durmieron" $
                    dormir "ieron" `shouldBe` Right (Just "durmieron")
                it "sintamos" $ sentir "amos" `shouldBe` Right (Just "sintamos")
                it "sintiendo" $
                    sentir "iendo" `shouldBe` Right (Just "sintiendo")
                it "sintieron" $
                    sentir "ieron" `shouldBe` Right (Just "sintieron")
            describe
                "not raise for diphthongizers when a diphthong is created when it otherwise would" $ do
                it "duermo" $ dormir "o" `shouldBe` Right (Just "duermo")
                it "duerma" $ dormir "a" `shouldBe` Right (Just "duerma")
                it "duerme" $ dormir "e" `shouldBe` Right (Just "duerme")
                it "siento" $ sentir "o" `shouldBe` Right (Just "siento")
                it "sienta" $ sentir "a" `shouldBe` Right (Just "sienta")
                it "siente" $ sentir "e" `shouldBe` Right (Just "siente")
            describe "misc" $ do
                it "should not start with a semivowel i" $
                    oler "o" `shouldBe` Right (Just "huelo")
                it "should not start with a semivowel u" $
                    errar "o" `shouldBe` Right (Just "yerro")
                it "should preserve a hard g in 'go' -> 'gue'" $
                    avergonzar "o" `shouldBe` Right (Just "avergüenzo")
                it
                    "not raise when the ending does not included ir or í (diphthong or not)" $
                    sentir "ido" `shouldBe` Right (Just "sentido")
        describe "parser + render" $
            prop
                "should give back the same result if parsed then rendered"
                (\x ->
                     case fmap render (p wordOnly x) of
                         Left _ -> True
                         Right s -> s == x)
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

type VerbHelper = String -> Either String (Maybe String)

p :: Parser String a -> String -> Either String a
p parser str = fmap fst (runParser parser str)

v :: Bool -> Bool -> String -> VerbHelper
v diph vR w e =
    fmap
        (fmap render)
        (liftA2 (conjugate diph vR) (p wordOnly w) (p endingOnly e))

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
