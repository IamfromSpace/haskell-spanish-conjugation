{-# LANGUAGE OverloadedStrings #-}

module Main where

import Anki.Collections.Default (CardData, initCol, insertCards)
import Anki.Init (analyzeAndIndex, createTables)
import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.Char (toLower)
import Database.SQLite.Simple (close, execute_, open)
import Linguistics.Conjugate
import Linguistics.FullWord
import qualified Linguistics.Parsers as LP
import Linguistics.Render
import Linguistics.Types
import qualified Parser as P
import Utils (swap, withLeft)

describe :: SubjectSensativeTense -> String
describe Conditional = "hoy si..."
describe Future = "mañana"
describe Imperfect = "hace varios años"
describe Present = "cada día"
describe Preterite = "ayer"
describe PresentSubjunctive = "quiero que..."
describe ImperfectSubjunctive = "yo quería que..."

conjugateToCardData ::
       String -> (SubjectSensativeTense, Subject) -> String -> CardData
conjugateToCardData infinitive (t, s) conjugated =
    let tenseDesc = describe t
        tense = fmap toLower (show t)
        subject = fmap toLower (show s)
    in ( "<img src=\"" ++
         subject ++
         ".png\" />" ++
         "<div style=\"font-size:36px; font-weight:bold\">" ++
         infinitive ++
         "</div><img src=\"" ++
         tense ++
         ".png\" /><div style=\"font-size:12px; font-style:italic\">(" ++
         tenseDesc ++ ", " ++ subject ++ ")</div>"
       , subject ++ " " ++ conjugated
       , infinitive ++ " -- " ++ show tense
       , [infinitive, tense, subject, "simple"])

type VerbConfig = (Bool, Bool, Bool, Bool, Bool, Bool)

--This should just really be the definition of toVerb
toVerb' :: FullWord -> Either String Verb
toVerb' = withLeft "word is not a verb!" . toVerb

parseVerb :: String -> Either String Verb
parseVerb = (fmap snd . P.runParser LP.wordOnly) >=> toVerb'

--This should just really be the definition of conjugate
conjugate' ::
       (VerbConfig, Verb)
    -> (SubjectSensativeTense, Subject)
    -> Either String FullWord
conjugate' = fmap (fmap (withLeft "could not conjugate")) (uncurry conjugate)

conjugate'' ::
       (VerbConfig, String)
    -> (SubjectSensativeTense, Subject)
    -> Either String FullWord
conjugate'' fwv st = swap (fmap parseVerb fwv) >>= flip conjugate' st

conjugate''' ::
       (VerbConfig, String)
    -> (SubjectSensativeTense, Subject)
    -> Either String CardData
conjugate''' (vc, str) st =
    fmap (conjugateToCardData str st . render) (conjugate'' (vc, str) st)

allTenses :: [(SubjectSensativeTense, Subject)]
allTenses =
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

configuredVerbToCardDatas :: (VerbConfig, String) -> Either String [CardData]
configuredVerbToCardDatas stringVerb =
    swap (fmap (conjugate''' stringVerb) allTenses)

mkCardDatas :: [(VerbConfig, String)] -> Either String [CardData]
mkCardDatas = foldr (liftA2 (++) . configuredVerbToCardDatas) (Right [])

verbs :: [(VerbConfig, String)]
verbs =
    [ ((False, False, False, False, False, False), "aprender")
    , ((False, False, False, False, False, False), "argüir")
    , ((False, False, False, False, False, False), "averiguar")
    , ((False, False, False, False, False, False), "beber")
    , ((False, False, False, False, False, False), "caminar")
    , ((False, False, False, False, False, False), "comer")
    , ((False, False, False, False, False, False), "construir")
    , ((False, False, False, False, False, False), "decidir")
    , ((False, False, False, False, False, False), "delinquir")
    , ((False, False, False, False, False, False), "distinguir")
    , ((False, False, False, False, False, False), "gozar")
    , ((False, False, False, False, False, False), "hablar")
    , ((False, False, False, False, False, False), "insistir")
    , ((False, False, False, False, False, False), "leer")
    -- TODO: (#10) Leading 'll' does not parse.
    --, ((False, False), "llevar")
    , ((False, False, False, False, False, False), "proteger")
    , ((False, False, False, False, False, False), "tocar")
    , ((False, False, False, False, False, False), "vencer")
    , ((False, False, False, False, False, False), "vivir")
    , ((False, False, False, False, False, True), "elegir")
    , ((False, False, False, False, False, True), "pedir")
    , ((False, False, False, False, False, True), "teñir")
    , ((False, False, False, False, True, False), "avergonzar")
    , ((False, False, False, False, True, False), "contar")
    , ((False, False, False, False, True, False), "errar")
    , ((False, False, False, False, True, False), "jugar")
    , ((False, False, False, False, True, False), "oler")
    , ((False, False, False, False, True, False), "pensar")
    , ((False, False, False, False, True, True), "dormir")
    , ((False, False, False, False, True, True), "sentir")
    , ((False, False, False, True, False, False), "enviar")
    , ((False, False, False, True, False, False), "aunar")
    , ((False, False, True, False, False, False), "conocer")
    , ((False, False, True, False, False, False), "nacer")
    , ((False, True, False, False, False, False), "asir")
    , ((False, True, False, False, False, False), "caer")
    ]

newDbWithCards :: String -> [CardData] -> IO ()
newDbWithCards fileName cardDatas = do
    conn <- open fileName
    createTables conn
    execute_ conn initCol
    insertCards conn 1398130088495 cardDatas
    analyzeAndIndex conn
    close conn

main :: IO ()
main =
    case mkCardDatas verbs of
        Left err -> error err
        Right cardDatas -> newDbWithCards "test.db" cardDatas
