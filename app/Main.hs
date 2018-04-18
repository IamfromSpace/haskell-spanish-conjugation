{-# LANGUAGE OverloadedStrings #-}

module Main where

import Anki.Collections.Default (CardData, initCol, insertCards)
import Anki.Init (analyzeAndIndex, createTables)
import Control.Applicative (liftA2)
import Data.Char (toLower)
import Database.SQLite.Simple (close, execute_, open)
import Linguistics.Conjugate
import Linguistics.Render
import Linguistics.Types
import Utils (swap)

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

conjugate' ::
       (VerbConfig, String)
    -> (SubjectSensativeTense, Subject)
    -> Either String CardData
conjugate' (vc, str) st =
    fmap (conjugateToCardData str st . render) (conjugate (vc, str) st)

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
    swap (fmap (conjugate' stringVerb) allTenses)

mkCardDatas :: [(VerbConfig, String)] -> Either String [CardData]
mkCardDatas = foldr (liftA2 (++) . configuredVerbToCardDatas) (Right [])

verbs :: [(VerbConfig, String)]
verbs =
    [ ((Nothing, False, False, False, False, False, False), "aprender")
    , ((Nothing, False, False, False, False, False, False), "argüir")
    , ((Nothing, False, False, False, False, False, False), "averiguar")
    , ((Nothing, False, False, False, False, False, False), "beber")
    , ((Nothing, False, False, False, False, False, False), "caminar")
    , ((Nothing, False, False, False, False, False, False), "comer")
    , ((Nothing, False, False, False, False, False, False), "construir")
    , ((Nothing, False, False, False, False, False, False), "decidir")
    , ((Nothing, False, False, False, False, False, False), "delinquir")
    , ((Nothing, False, False, False, False, False, False), "distinguir")
    , ((Nothing, False, False, False, False, False, False), "gozar")
    , ((Nothing, False, False, False, False, False, False), "hablar")
    , ((Nothing, False, False, False, False, False, False), "insistir")
    , ((Nothing, False, False, False, False, False, False), "leer")
    -- TODO: (#10) Leading 'll' does not parse.
    --, ((False, False), "llevar")
    , ((Nothing, False, False, False, False, False, False), "proteger")
    , ((Nothing, False, False, False, False, False, False), "tocar")
    , ((Nothing, False, False, False, False, False, False), "vencer")
    , ((Nothing, False, False, False, False, False, False), "vivir")
    , ((Nothing, False, False, False, False, False, True), "elegir")
    , ((Nothing, False, False, False, False, False, True), "pedir")
    , ((Nothing, False, False, False, False, False, True), "teñir")
    , ((Nothing, False, False, False, False, True, False), "avergonzar")
    , ((Nothing, False, False, False, False, True, False), "contar")
    , ((Nothing, False, False, False, False, True, False), "errar")
    , ((Nothing, False, False, False, False, True, False), "jugar")
    , ((Nothing, False, False, False, False, True, False), "oler")
    , ((Nothing, False, False, False, False, True, False), "pensar")
    , ((Nothing, False, False, False, False, True, True), "dormir")
    , ((Nothing, False, False, False, False, True, True), "sentir")
    , ((Nothing, False, False, False, True, False, False), "enviar")
    , ((Nothing, False, False, False, True, False, False), "aunar")
    , ((Nothing, False, False, True, False, False, False), "conocer")
    , ((Nothing, False, False, True, False, False, False), "nacer")
    , ((Nothing, False, True, False, False, False, False), "asir")
    , ((Nothing, False, True, False, False, False, False), "caer")
    , ((Nothing, True, True, False, False, False, False), "salir")
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
