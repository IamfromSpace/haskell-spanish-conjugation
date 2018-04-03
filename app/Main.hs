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
    in ( "<div style=\"font-size:36px; font-weight:bold\">" ++
         infinitive ++
         "</div><img src=\"" ++
         tense ++
         ".png\" /><img src=\"" ++
         subject ++
         ".png\" /><div style=\"font-size:12px; font-style:italic\">(" ++
         tenseDesc ++ ", " ++ subject ++ ")</div>"
       , subject ++ " " ++ conjugated
       , infinitive ++ " -- " ++ show tense
       , [infinitive, tense, subject, "simple"])

type VerbConfig = (Bool, Bool)

--This should just really be the definition of toVerb
toVerb' :: FullWord -> Either String Verb
toVerb' = withLeft "word is not a verb!" . toVerb

parseVerb :: String -> Either String Verb
parseVerb = (fmap fst . P.runParser LP.wordOnly) >=> toVerb'

--This should just really be the definition of conjugate
conjugate' ::
       (VerbConfig, Verb)
    -> (SubjectSensativeTense, Subject)
    -> Either String FullWord
conjugate' =
    fmap (fmap (withLeft "could not conjugate")) (uncurry (uncurry conjugate))

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
        [Yo, Tú, Usted, Nosotros, Ustedes]

configuredVerbToCardDatas :: (VerbConfig, String) -> Either String [CardData]
configuredVerbToCardDatas stringVerb =
    swap (fmap (conjugate''' stringVerb) allTenses)

mkCardDatas :: [(VerbConfig, String)] -> Either String [CardData]
mkCardDatas = foldr (liftA2 (++) . configuredVerbToCardDatas) (Right [])

verbs :: [(VerbConfig, String)]
verbs =
    [ ((False, False), "aprender")
    , ((False, False), "argüir")
    , ((False, False), "averiguar")
    , ((False, False), "beber")
    , ((False, False), "caminar")
    , ((False, False), "comer")
    , ((False, False), "construir")
    , ((False, False), "decidir")
    , ((False, False), "delinquir")
    , ((False, False), "distinguir")
    , ((False, False), "gozar")
    , ((False, False), "hablar")
    , ((False, False), "insistir")
    , ((False, False), "leer")
    -- TODO: (#10) Leading 'll' does not parse.
    --, ((False, False), "llevar")
    , ((False, False), "proteger")
    , ((False, False), "tocar")
    , ((False, False), "vencer")
    , ((False, False), "vivir")
    , ((False, True), "elegir")
    , ((False, True), "pedir")
    , ((False, True), "teñir")
    , ((True, False), "avergonzar")
    , ((True, False), "contar")
    , ((True, False), "errar")
    , ((True, False), "jugar")
    , ((True, False), "oler")
    , ((True, False), "pensar")
    , ((True, True), "dormir")
    , ((True, True), "sentir")
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
