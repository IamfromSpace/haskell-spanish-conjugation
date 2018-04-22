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
       (VerbConfig String String, String)
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

configuredVerbToCardDatas ::
       (VerbConfig String String, String) -> Either String [CardData]
configuredVerbToCardDatas stringVerb =
    swap (fmap (conjugate' stringVerb) allTenses)

mkCardDatas :: [(VerbConfig String String, String)] -> Either String [CardData]
mkCardDatas = foldr (liftA2 (++) . configuredVerbToCardDatas) (Right [])

verbs :: [(VerbConfig String String, String)]
verbs =
    [ ((Nothing, Nothing, False, False, False, False, False, False), "aprender")
    , ((Nothing, Nothing, False, False, False, False, False, False), "argüir")
    , ( (Nothing, Nothing, False, False, False, False, False, False)
      , "averiguar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "beber")
    , ((Nothing, Nothing, False, False, False, False, False, False), "caminar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "comer")
    , ( (Nothing, Nothing, False, False, False, False, False, False)
      , "construir")
    , ((Nothing, Nothing, False, False, False, False, False, False), "decidir")
    , ( (Nothing, Nothing, False, False, False, False, False, False)
      , "delinquir")
    , ( (Nothing, Nothing, False, False, False, False, False, False)
      , "distinguir")
    , ((Nothing, Nothing, False, False, False, False, False, False), "gozar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "hablar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "insistir")
    , ((Nothing, Nothing, False, False, False, False, False, False), "leer")
    -- TODO: (#10) Leading 'll' does not parse.
    --, ((False, False), "llevar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "proteger")
    , ((Nothing, Nothing, False, False, False, False, False, False), "tocar")
    , ((Nothing, Nothing, False, False, False, False, False, False), "vencer")
    , ((Nothing, Nothing, False, False, False, False, False, False), "vivir")
    , ((Nothing, Nothing, False, False, False, False, False, True), "elegir")
    , ((Nothing, Nothing, False, False, False, False, False, True), "pedir")
    , ((Nothing, Nothing, False, False, False, False, False, True), "teñir")
    , ( (Nothing, Nothing, False, False, False, False, True, False)
      , "avergonzar")
    , ((Nothing, Nothing, False, False, False, False, True, False), "contar")
    , ((Nothing, Nothing, False, False, False, False, True, False), "errar")
    , ((Nothing, Nothing, False, False, False, False, True, False), "jugar")
    , ((Nothing, Nothing, False, False, False, False, True, False), "oler")
    , ((Nothing, Nothing, False, False, False, False, True, False), "pensar")
    , ((Nothing, Nothing, False, False, False, False, True, True), "dormir")
    , ((Nothing, Nothing, False, False, False, False, True, True), "sentir")
    , ((Nothing, Nothing, False, False, False, True, False, False), "enviar")
    , ((Nothing, Nothing, False, False, False, True, False, False), "aunar")
    , ((Nothing, Nothing, False, False, True, False, False, False), "conocer")
    , ((Nothing, Nothing, False, False, True, False, False, False), "nacer")
    , ((Nothing, Nothing, False, True, False, False, False, False), "asir")
    , ((Nothing, Nothing, False, True, False, False, False, False), "caer")
    , ((Nothing, Nothing, True, True, False, False, False, False), "salir")
    , ( (Nothing, Just (True, "uv"), True, True, False, False, True, False)
      , "tener")
    , ( (Nothing, Just (True, "ud"), True, False, False, False, True, False)
      , "poder")
    , ( (Nothing, Just (True, "is"), True, False, False, False, True, False)
      , "querer")
    , ( (Nothing, Just (False, "uv"), False, False, False, False, False, False)
      , "andar")
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
