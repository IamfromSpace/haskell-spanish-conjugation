import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)

import Control.Applicative
import Data.Maybe (isJust)
import Linguistics.Conjugate
import Linguistics.FullWord (toVerb)
import Linguistics.HandleIrregularPreterite
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
                    tocar (PresentSubjunctive, Nosotros) `shouldBe` "toquemos"
                it "handles c -> qu from a -> é" $
                    tocar (Preterite, Yo) `shouldBe` "toqué"
                it "handles z -> c from a -> e" $
                    gozar (PresentSubjunctive, Nosotros) `shouldBe` "gocemos"
                it "handles z -> c from a -> é" $
                    gozar (Preterite, Yo) `shouldBe` "gocé"
                it "handles u -> ü from a -> e" $
                    averiguar (PresentSubjunctive, Nosotros) `shouldBe`
                    "averigüemos"
                it "handles qu -> c from e -> a" $
                    delinquir (PresentSubjunctive, Nosotros) `shouldBe`
                    "delincamos"
                it "handles c -> z from e -> o" $
                    vencer (Present, Yo) `shouldBe` "venzo"
                it "handles g -> j from e -> a" $
                    proteger (PresentSubjunctive, Nosotros) `shouldBe`
                    "protejamos"
                it "handles gu -> g from i -> a" $
                    distinguir (PresentSubjunctive, Nosotros) `shouldBe`
                    "distingamos"
                it "handles +y in uir -> o" $
                    construir (Present, Yo) `shouldBe` "construyo"
                it "handles +y and -ü from üir -> o" $
                    argüir (Present, Yo) `shouldBe` "arguyo"
            describe "handles no special rules for..." $ do
                it "averiguar + amos" $
                    averiguar (Present, Nosotros) `shouldBe` "averiguamos"
                it "hablar + emos" $
                    hablar (PresentSubjunctive, Nosotros) `shouldBe` "hablemos"
                it "distinguir + es" $
                    distinguir (Present, Tú) `shouldBe` "distingues"
                it "correr + amos" $
                    correr (PresentSubjunctive, Nosotros) `shouldBe` "corramos"
                it "construir + ía" $
                    construir (Imperfect, Usted) `shouldBe` "construía"
                it "argüir + ía" $ argüir (Imperfect, Usted) `shouldBe` "argüía"
            describe "accent i when stressed" $ do
                it "0" $ caer (Preterite, Nosotros) `shouldBe` "caímos"
                it "1" $ caer (Preterite, Tú) `shouldBe` "caíste"
                it "2" $ leer (Preterite, Nosotros) `shouldBe` "leímos"
                it "3" $ oír (Preterite, Nosotros) `shouldBe` "oímos"
            describe "remove accents in monosyllabic forms" $ do
                it "0" $ liar (Preterite, Yo) `shouldBe` "lie"
                it "1" $ liar (Preterite, Usted) `shouldBe` "lio"
                it "2" $ ver (Preterite, Yo) `shouldBe` "vi"
                it "3" $ ver (Preterite, Usted) `shouldBe` "vio"
            describe "replace an semivowel left i with a y between cores" $ do
                it "0" $ caer (Preterite, Usted) `shouldBe` "cayó"
                it "1" $ caer (Preterite, Ustedes) `shouldBe` "cayeron"
                it "2" $ construir (Preterite, Usted) `shouldBe` "construyó"
                it "3" $ argüir PresentParticiple `shouldBe` "arguyendo"
            describe
                "not replace an unstressed i with a y between a silent vowels" $ do
                it "0" $ delinquir (Preterite, Usted) `shouldBe` "delinquió"
                it "1" $
                    delinquir (Preterite, Ustedes) `shouldBe` "delinquieron"
                it "2" $ distinguir (Preterite, Usted) `shouldBe` "distinguió"
                it "3" $
                    distinguir (Preterite, Ustedes) `shouldBe` "distinguieron"
            describe "drop unneeded semi-vowel left" $ do
                it "drop a semivowel left i after a ll" $
                    bullir (Preterite, Usted) `shouldBe` "bulló"
                it "drop a semivowel left i after a ñ" $
                    tañer (Preterite, Usted) `shouldBe` "tañó"
            describe "should stem change if stressed" $ do
                it "e" $ pensar (Present, Yo) `shouldBe` "pienso"
                it "o" $ contar (Present, Yo) `shouldBe` "cuento"
                it "i" $ adquirir (Present, Yo) `shouldBe` "adquiero"
                it "u" $ jugar (Present, Yo) `shouldBe` "juego"
            describe "should not stem change if not stressed" $ do
                it "e" $ pensar (Present, Nosotros) `shouldBe` "pensamos"
                it "o" $ contar (Present, Nosotros) `shouldBe` "contamos"
                it "i" $ adquirir (Present, Nosotros) `shouldBe` "adquirimos"
                it "u" $ jugar (Present, Nosotros) `shouldBe` "jugamos"
            describe "should not stem change when ending is explicitly accented" $ do
                it "e" $ pensar (Preterite, Usted) `shouldBe` "pensó"
                it "o" $ contar (Preterite, Usted) `shouldBe` "contó"
                it "i" $ adquirir (Preterite, Usted) `shouldBe` "adquirió"
                it "u" $ jugar (Preterite, Usted) `shouldBe` "jugó"
            describe
                "raise for non-diphthongizers when the ending doesn't have ir or í" $ do
                it "o" $ pedir (Present, Yo) `shouldBe` "pido"
                it "a" $ pedir (PresentSubjunctive, Usted) `shouldBe` "pida"
                it "e" $ pedir (Present, Usted) `shouldBe` "pide"
            describe
                "raise for diphthongizers when no diphthong is created and ending does not included ir or í" $ do
                it "durmamos" $
                    dormir (PresentSubjunctive, Nosotros) `shouldBe` "durmamos"
                it "durmiendo" $ dormir PresentParticiple `shouldBe` "durmiendo"
                it "durmieron" $
                    dormir (Preterite, Ustedes) `shouldBe` "durmieron"
                it "sintamos" $
                    sentir (PresentSubjunctive, Nosotros) `shouldBe` "sintamos"
                it "sintiendo" $ sentir PresentParticiple `shouldBe` "sintiendo"
                it "sintieron" $
                    sentir (Preterite, Ustedes) `shouldBe` "sintieron"
            describe
                "not raise for diphthongizers when a diphthong is created when it otherwise would" $ do
                it "duermo" $ dormir (Present, Yo) `shouldBe` "duermo"
                it "duerma" $
                    dormir (PresentSubjunctive, Usted) `shouldBe` "duerma"
                it "duerme" $ dormir (Present, Usted) `shouldBe` "duerme"
                it "siento" $ sentir (Present, Yo) `shouldBe` "siento"
                it "sienta" $
                    sentir (PresentSubjunctive, Usted) `shouldBe` "sienta"
                it "siente" $ sentir (Present, Usted) `shouldBe` "siente"
            describe "diphthong breaking" $ do
                it "should break enviar on present yo" $
                    enviar (Present, Yo) `shouldBe` "envío"
                it "should not break enviar on preterite tú" $
                    enviar (Preterite, Tú) `shouldBe` "enviaste"
                it
                    "should not break enviar on explicit stress (preterite usted)" $
                    enviar (Preterite, Usted) `shouldBe` "envió"
                it "should break aislar on present yo" $
                    aislar (Present, Yo) `shouldBe` "aíslo"
                it "should break aunar on present yo" $
                    aunar (Present, Yo) `shouldBe` "aúno"
                it "should break descafeinar on present yo" $
                    descafeinar (Present, Yo) `shouldBe` "descafeíno"
                -- TODO: Rehusar (once 'h's are parsed properly)
            describe "zc verbs" $ do
                it "should insert a z on present yo" $
                    conocer (Present, Yo) `shouldBe` "conozco"
                it "should insert a z on on subjunctive present tú" $
                    conocer (PresentSubjunctive, Tú) `shouldBe` "conozcas"
                it
                    "should not insert a z on on subjunctive present tú, even on a zc word" $
                    conocer (Present, Tú) `shouldBe` "conoces"
                it "should not add a z on an unaffected verb in present yo" $
                    torcer (Present, Yo) `shouldBe` "tuerzo"
                -- TODO: -cir ending verb when they're complete
                -- (they all seem to have irregular preterite stems)
            describe "yo-go verbs" $
                -- Untested, but theoretically would support a yo-go verb that
                -- both vowel raises and diphthongizes, for example:
                -- fakewordocer -> yo fakewordugo, usted fakeworduece, usted fakeworduciera
                -- However, I can't find a word that does.
             do
                it "should insert an ig between vowels on present yo" $
                    caer (Present, Yo) `shouldBe` "caigo"
                it "should insert a g on a s on present yo" $
                    asir (Present, Yo) `shouldBe` "asgo"
                it "should convert a c to a g on present yo (and vowel raise)" $
                    decir (Present, Yo) `shouldBe` "digo"
                it
                    "should insert a g on an n on present yo (and not diphthongize)" $
                    tener (Present, Yo) `shouldBe` "tengo"
                it "should insert a g on an l on present yo" $
                    salir (Present, Yo) `shouldBe` "salgo"
                it
                    "should insert an ig between vowels on subjunctive present yo" $
                    caer (PresentSubjunctive, Yo) `shouldBe` "caiga"
                it "should insert a g on a s on subjunctive present yo" $
                    asir (PresentSubjunctive, Yo) `shouldBe` "asga"
                it
                    "should convert a c to a g on subjunctive present yo (and vowel raise)" $
                    decir (PresentSubjunctive, Yo) `shouldBe` "diga"
                it
                    "should insert a g on an n on subjunctive present yo (and not diphthongize)" $
                    tener (PresentSubjunctive, Yo) `shouldBe` "tenga"
                it "should insert a g on an l on subjunctive present yo" $
                    salir (PresentSubjunctive, Yo) `shouldBe` "salga"
                it "should should not insert a g on present tú for caer" $
                    caer (Present, Tú) `shouldBe` "caes"
                it "should should not insert a g on present tú for asir" $
                    asir (Present, Tú) `shouldBe` "ases"
                it "should should not insert a g on present tú for decir" $
                    decir (Present, Tú) `shouldBe` "dices"
                it "should should not insert a g on present tú for tener" $
                    tener (Present, Tú) `shouldBe` "tienes"
                it "should should not insert a g on present tú for salir" $
                    salir (Present, Tú) `shouldBe` "sales"
            describe "infinitive shortening verbs" $ do
                it "should drop the e between br in the future" $
                    saber (Future, Nosotros) `shouldBe` "sabremos"
                it "should drop the e between dr in the future" $
                    poder (Future, Nosotros) `shouldBe` "podremos"
                it "should drop the e between rr in the future" $
                    querer (Future, Nosotros) `shouldBe` "querremos"
                it "should replace e with a d between nr in the future" $
                    tener (Future, Nosotros) `shouldBe` "tendremos"
                it "should replace i with a d between lr in the future" $
                    salir (Future, Nosotros) `shouldBe` "saldremos"
                it "should drop -ce in the future" $
                    hacer (Future, Nosotros) `shouldBe` "haremos"
                it "should drop -ec in the future" $
                    decir (Future, Nosotros) `shouldBe` "diremos"
                it "should not drop the e between br if there's a diphthong" $
                    saber (Preterite, Ustedes) `shouldBe` "sabieron"
                it "should not drop the i between lr if there's a diphthong" $
                    salir (Preterite, Ustedes) `shouldBe` "salieron"
                it
                    "should not drop the e between br if it's actually the infinitive" $
                    saber Infinitive `shouldBe` "saber"
                it "should not drop the e if there is no r in the ending" $
                    saber (Present, Nosotros) `shouldBe` "sabemos"
            describe "irregular preterite" $ do
                it "should be tuvo" $ tener (Preterite, Yo) `shouldBe` "tuvo"
                it "should be tuve" $ tener (Preterite, Usted) `shouldBe` "tuve"
                it "should be tuvimos" $
                    tener (Preterite, Nosotros) `shouldBe` "tuvimos"
                it "should be tuviera" $
                    tener (ImperfectSubjunctive, Yo) `shouldBe` "tuviera"
            describe "misc" $ do
                it "should not start with a semivowel i" $
                    oler (Present, Yo) `shouldBe` "huelo"
                it "should not start with a semivowel u" $
                    errar (Present, Yo) `shouldBe` "yerro"
                it "should preserve a hard g in 'go' -> 'gue'" $
                    avergonzar (Present, Yo) `shouldBe` "avergüenzo"
                it
                    "not raise when the ending does not included ir or í (diphthong or not)" $
                    sentir PastParticiple `shouldBe` "sentido"
        describe "parser + render" $
            prop
                "should give back the same result if parsed then rendered"
                (\x ->
                     case runParser wordOnly x of
                         Right ("", s) -> render s == x
                         _ -> True)
        describe "getEnding" $ do
            it "does not throw for subject sensative ending" $
                -- There may be a more clever way to make sure this is exhaustive
                -- but notably, if a new tense is added, the type checker won't let us know
                let allTenses =
                        liftA2
                            (,)
                            [ Conditional
                            , Future
                            , Imperfect
                            , Present
                            , Preterite
                            , PresentSubjunctive
                            , ImperfectSubjunctive
                            ]
                            [Yo, Tú, Usted, Él, Nosotros, Ustedes, Ellos]
                    allEndings = liftA2 getEnding [AR, ER, IR] allTenses
                    -- Evaluation of the list must be force, done here by checking for a coda
                in length (filter (\(_, _, x) -> isJust x) allEndings) `shouldBe`
                   81
                        --[Infinitive, PastParticiple, PresentParticiple] ++
            it "does not throw for subject insensative ending" $
                let allEndings =
                        liftA2
                            getEnding
                            [AR, ER, IR]
                            [Infinitive, PastParticiple, PresentParticiple]
                    -- Evaluation of the list must be force, done here by checking for a coda
                in length (filter (\(_, _, x) -> isJust x) allEndings) `shouldBe`
                   3

type VerbHelper a = a -> String

-- TODO: these are probably _close_ to being useful lib functions
cong ::
       (HasVerbEnding a, HandlesIrregularPreterite a)
    => (Maybe (Bool, (Core, InnerCluster)), Bool, Bool, Bool, Bool, Bool, Bool)
    -> FullWord
    -> a
    -> Maybe String
cong vConf fw st = fmap render (toVerb fw >>= flip (conjugate vConf) st)

-- This is not total, but that's actually perfect for our tests
v :: (HasVerbEnding a, HandlesIrregularPreterite a)
  => (Maybe (Bool, (Core, InnerCluster)), Bool, Bool, Bool, Bool, Bool, Bool)
  -> String
  -> VerbHelper a
v vConf str st =
    let renderedParser = fmap (\fw -> cong vConf fw st) wordOnly
    in case runParser renderedParser str of
           Right ("", Just rendered) -> rendered
           Right (_:_, Just _)
               -- This case really should be impossible by using `wordOnly`
            ->
               error
                   "Test inputs are invalid!  Verb string was not fully parsed!"
           Right (_, Nothing) ->
               error
                   "Test inputs are invalid!  Verb had no valid conjugation (did not end with ar/er/ir, could not diphthongize, etc)!"
           Left _ ->
               error "Test inputs are invalid!  Verb string was not parseable!"

tocar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
tocar = v (Nothing, False, False, False, False, False, False) "tocar"

gozar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
gozar = v (Nothing, False, False, False, False, False, False) "gozar"

averiguar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
averiguar = v (Nothing, False, False, False, False, False, False) "averiguar"

delinquir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
delinquir = v (Nothing, False, False, False, False, False, False) "delinquir"

vencer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
vencer = v (Nothing, False, False, False, False, False, False) "vencer"

proteger :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
proteger = v (Nothing, False, False, False, False, False, False) "proteger"

distinguir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
distinguir = v (Nothing, False, False, False, False, False, False) "distinguir"

construir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
construir = v (Nothing, False, False, False, False, False, False) "construir"

argüir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
argüir = v (Nothing, False, False, False, False, False, False) "argüir"

hablar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
hablar = v (Nothing, False, False, False, False, False, False) "hablar"

correr :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
correr = v (Nothing, False, False, False, False, False, False) "correr"

caer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
caer = v (Nothing, False, True, False, False, False, False) "caer"

leer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
leer = v (Nothing, False, False, False, False, False, False) "leer"

oír :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
oír = v (Nothing, False, False, False, False, False, False) "oír"

liar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
liar = v (Nothing, False, False, False, False, False, False) "liar"

ver :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
ver = v (Nothing, False, False, False, False, False, False) "ver"

bullir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
bullir = v (Nothing, False, False, False, False, False, False) "bullir"

tañer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
tañer = v (Nothing, False, False, False, False, False, False) "tañer"

pensar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
pensar = v (Nothing, False, False, False, False, True, False) "pensar"

contar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
contar = v (Nothing, False, False, False, False, True, False) "contar"

adquirir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
adquirir = v (Nothing, False, False, False, False, True, False) "adquirir"

jugar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
jugar = v (Nothing, False, False, False, False, True, False) "jugar"

oler :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
oler = v (Nothing, False, False, False, False, True, False) "oler"

errar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
errar = v (Nothing, False, False, False, False, True, False) "errar"

avergonzar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
avergonzar = v (Nothing, False, False, False, False, True, False) "avergonzar"

pedir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
pedir = v (Nothing, False, False, False, False, False, True) "pedir"

dormir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
dormir = v (Nothing, False, False, False, False, True, True) "dormir"

sentir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
sentir = v (Nothing, False, False, False, False, True, True) "sentir"

enviar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
enviar = v (Nothing, False, False, False, True, False, False) "enviar"

aislar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
aislar = v (Nothing, False, False, False, True, False, False) "aislar"

aunar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
aunar = v (Nothing, False, False, False, True, False, False) "aunar"

descafeinar :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
descafeinar = v (Nothing, False, False, False, True, False, False) "descafeinar"

conocer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
conocer = v (Nothing, False, False, True, False, False, False) "conocer"

torcer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
torcer = v (Nothing, False, False, False, False, True, False) "torcer"

asir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
asir = v (Nothing, False, True, False, False, False, False) "asir"

-- NOTE:  Not all rules for this verb are currently supported!
decir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
decir = v (Nothing, True, True, False, False, False, True) "decir"

tener :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
tener =
    v
        ( Just
              ( True
              , ((False, (Nothing, Left U)), Just (Nothing, Single (Regular V))))
        , True
        , True
        , False
        , False
        , True
        , False)
        "tener"

salir :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
salir = v (Nothing, True, True, False, False, False, False) "salir"

-- NOTE:  Not all rules for this verb are currently supported!
saber :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
saber = v (Nothing, True, False, False, False, False, False) "saber"

-- NOTE:  Not all rules for this verb are currently supported!
poder :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
poder = v (Nothing, True, False, False, False, True, False) "poder"

-- NOTE:  Not all rules for this verb are currently supported!
querer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
querer = v (Nothing, True, False, False, False, True, False) "querer"

-- NOTE:  Not all rules for this verb are currently supported!
hacer :: (HasVerbEnding a, HandlesIrregularPreterite a) => VerbHelper a
hacer = v (Nothing, True, True, False, False, False, False) "hacer"
