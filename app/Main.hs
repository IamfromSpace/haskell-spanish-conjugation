{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (liftA2, pure)
import Control.Monad ((>=>))
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord, toLower)
import Database.SQLite.Simple
       (NamedParam((:=)), Query, close, executeNamed, execute_, open)
import Database.SQLite.Simple.Internal (Connection)
import Linguistics.Conjugate
import Linguistics.FullWord
import qualified Linguistics.Parsers as LP
import Linguistics.Render
import Linguistics.Types
import qualified Parser as P
import Utils (swap, withLeft)

createCol :: Query
createCol =
    "CREATE TABLE col (\n\
   \ id              integer primary key,\n\
   \ crt             integer not null,\n\
   \ mod             integer not null,\n\
   \ scm             integer not null,\n\
   \ ver             integer not null,\n\
   \ dty             integer not null,\n\
   \ usn             integer not null,\n\
   \ ls              integer not null,\n\
   \ conf            text not null,\n\
   \ models          text not null,\n\
   \ decks           text not null,\n\
   \ dconf           text not null,\n\
   \ tags            text not null\n\
\ );"

initCol :: Query
initCol =
    "INSERT INTO col VALUES(1,1332961200,1398130163295,1398130163168,11,0,0,0,'{\"nextPos\": 1, \"estTimes\": true, \"activeDecks\": [1], \"sortType\": \"noteFld\", \"timeLim\": 0, \"sortBackwards\": false, \"addToCur\": true, \"curDeck\": 1, \"newBury\": true, \"newSpread\": 0, \"dueCounts\": true, \"curModel\": \"1398130163168\", \"collapseTime\": 1200}','{\"1342697561419\": {\"vers\": [], \"name\": \"Basic\", \"tags\": [], \"did\": 1398130078204, \"usn\": -1, \"req\": [[0, \"all\", [0]]], \"flds\": [{\"name\": \"Front\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 0, \"font\": \"Arial\", \"size\": 12}, {\"name\": \"Back\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 1, \"font\": \"Arial\", \"size\": 12}], \"sortf\": 0, \"latexPre\": \"\\\\documentclass[12pt]{article}\\n\\\\special{papersize=3in,5in}\\n\\\\usepackage{amssymb,amsmath}\\n\\\\pagestyle{empty}\\n\\\\setlength{\\\\parindent}{0in}\\n\\\\begin{document}\\n\", \"tmpls\": [{\"name\": \"Forward\", \"qfmt\": \"{{Front}}\", \"did\": null, \"bafmt\": \"\", \"afmt\": \"{{FrontSide}}\\n\\n\\n<hr id=answer/>\\n\\n{{Back}}\", \"ord\": 0, \"bqfmt\": \"\"}], \"latexPost\": \"\\\\end{document}\", \"type\": 0, \"id\": 1342697561419, \"css\": \".card {\\n font-family: arial;\\n font-size: 30px;\\n text-align: center;\\n color: black;\\n background-color: white;\\n}\\n\\n.card1 { background-color: #FFFFFF; }\", \"mod\": 1398130117}}','{\"1\": {\"desc\": \"\", \"name\": \"Default\", \"extendRev\": 50, \"usn\": 0, \"collapsed\": false, \"newToday\": [0, 0], \"timeToday\": [0, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [0, 0], \"lrnToday\": [0, 0], \"id\": 1, \"mod\": 1398130160}, \"1398130078204\": {\"desc\": \"\", \"name\": \"Spanish Haskell Conjugation\", \"extendRev\": 50, \"usn\": -1, \"collapsed\": false, \"newToday\": [754, 0], \"timeToday\": [754, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [754, 0], \"lrnToday\": [754, 0], \"id\": 1398130078204, \"mod\": 1398130140}}','{\"1\": {\"name\": \"Default\", \"replayq\": true, \"lapse\": {\"leechFails\": 8, \"minInt\": 1, \"delays\": [10], \"leechAction\": 0, \"mult\": 0}, \"rev\": {\"perDay\": 100, \"fuzz\": 0.05, \"ivlFct\": 1, \"maxIvl\": 36500, \"ease4\": 1.3, \"bury\": true, \"minSpace\": 1}, \"timer\": 0, \"maxTaken\": 60, \"usn\": 0, \"new\": {\"perDay\": 20, \"delays\": [1, 10], \"separate\": true, \"ints\": [1, 4, 7], \"initialFactor\": 2500, \"bury\": true, \"order\": 1}, \"mod\": 0, \"id\": 1, \"autoplay\": true}}','{}');\n"

createNotes :: Query
createNotes =
    "CREATE TABLE notes (\n\
  \ id              integer primary key,\n\
  \ guid            text not null,\n\
  \ mid             integer not null,\n\
  \ mod             integer not null,\n\
  \ usn             integer not null,\n\
  \ tags            text not null,\n\
  \ flds            text not null,\n\
  \ sfld            integer not null,\n\
  \ csum            integer not null,\n\
  \ flags           integer not null,\n\
  \ data            text not null\n\
  \ );"

createCards :: Query
createCards =
    "CREATE TABLE cards (\n\
  \ id              integer primary key,\n\
  \ nid             integer not null,\n\
  \ did             integer not null,\n\
  \ ord             integer not null,\n\
  \ mod             integer not null,\n\
  \ usn             integer not null,\n\
  \ type            integer not null,\n\
  \ queue           integer not null,\n\
  \ due             integer not null,\n\
  \ ivl             integer not null,\n\
  \ factor          integer not null,\n\
  \ reps            integer not null,\n\
  \ lapses          integer not null,\n\
  \ left            integer not null,\n\
  \ odue            integer not null,\n\
  \ odid            integer not null,\n\
  \ flags           integer not null,\n\
  \ data            text not null\n\
  \ );"

createRevLog :: Query
createRevLog =
    "CREATE TABLE revlog (\n\
  \ id              integer primary key,\n\
  \ cid             integer not null,\n\
  \ usn             integer not null,\n\
  \ ease            integer not null,\n\
  \ ivl             integer not null,\n\
  \ lastIvl         integer not null,\n\
  \ factor          integer not null,\n\
  \ time            integer not null,\n\
  \ type            integer not null\n\
  \ );"

createGraves :: Query
createGraves =
    "CREATE TABLE graves (\n\
  \ usn             integer not null,\n\
  \ oid             integer not null,\n\
  \ type            integer not null\n\
  \ );"

insertNote :: Query
insertNote =
    "INSERT INTO notes VALUES(:noteId,:noteGuid,1342697561419,:noteId,-1,:tags,:content,:searchValue,:checkSum,0,'');"

insertCard :: Query
insertCard =
    "INSERT INTO cards VALUES(:cardId,:noteId,1398130078204,0,:cardId,-1,0,0,:noteId,0,0,0,0,0,0,0,0,'');"

analyzeAndIndex :: Query
analyzeAndIndex =
    "ANALYZE sqlite_master;\n\
  \ INSERT INTO \"sqlite_stat1\" VALUES('col',NULL,'1');\n\
  \ CREATE INDEX ix_notes_usn on notes (usn);\n\
  \ CREATE INDEX ix_cards_usn on cards (usn);\n\
  \ CREATE INDEX ix_revlog_usn on revlog (usn);\n\
  \ CREATE INDEX ix_cards_nid on cards (nid);\n\
  \ CREATE INDEX ix_cards_sched on cards (did, queue, due);\n\
  \ CREATE INDEX ix_revlog_cid on revlog (cid);\n\
  \ CREATE INDEX ix_notes_csum on notes (csum);"

getAnkiCheckSum :: String -> Int
getAnkiCheckSum s
    --Sha1 hash >>> first 4 bytes >>> as an Int
 = BS.foldl (\p n -> 256 * p + ord n) 0 (BS.take 4 (hash (BS.pack s)))

type CardData = (String, String, String, [String])

-- Seems to indicate I need a different strategy for SimpleTense
-- should possibly be Tense/Subject totally separate and then
-- Conjugate which is pair?
-- This really should be a Maybe, but I'm lazy, which helps show that
-- my data types aren't correct... The ability to fail should be avoided if possible.
tenseToStrTuple :: SimpleTense -> (String, String, String)
tenseToStrTuple (Conditional s) =
    ("conditional", toLower <$> show s, "hoy si...")
tenseToStrTuple (Future s) = ("future", toLower <$> show s, "mañana")
tenseToStrTuple (Imperfect s) =
    ("imperfect", toLower <$> show s, "hace varios años")
tenseToStrTuple (Present s) = ("present", toLower <$> show s, "cada día")
tenseToStrTuple (Preterite s) = ("preterite", toLower <$> show s, "ayer")
tenseToStrTuple (PresentSubjunctive s) =
    ("present_subjunctive", toLower <$> show s, "quiero que...")
tenseToStrTuple (ImperfectSubjunctive s) =
    ("imperfect_subjunctive", toLower <$> show s, "yo quería que...")
tenseToStrTuple x = (show x, "", "")

conjugateToCardData :: String -> SimpleTense -> String -> CardData
conjugateToCardData infinitive tense conjugated =
    let (time, subject, tense_desc) = tenseToStrTuple tense
    in ( "<div style=\"font-size:36px; font-weight:bold\">" ++
         infinitive ++
         "</div><img src=\"" ++
         time ++
         ".png\" /><img src=\"" ++
         subject ++
         ".png\" /><div style=\"font-size:12px; font-style:italic\">(" ++
         tense_desc ++ ", " ++ subject ++ ")</div>"
       , subject ++ " " ++ conjugated
       , infinitive ++ " -- " ++ show tense
       , [infinitive, time, subject, "simple"])

spaceToUnderscore :: Char -> Char
spaceToUnderscore x =
    if x == ' '
        then '_'
        else x

getNamedParamTuple :: Int -> CardData -> ([NamedParam], [NamedParam])
getNamedParamTuple ts (front, back, searchValue, tags) =
    ( [ ":content" := front ++ "\US" ++ back
      , ":searchValue" := searchValue
      , ":tags" := unwords (fmap (fmap spaceToUnderscore) tags)
      , ":checkSum" := getAnkiCheckSum searchValue
      , ":noteGuid" := show ts
      , ":noteId" := ts
      ]
    , [":cardId" := ts + 1, ":noteId" := ts])

insertCards :: Connection -> Int -> [CardData] -> IO ()
insertCards conn j =
    let reducer cd (io, i) =
            let (note, card) = getNamedParamTuple i cd
            in ( io *> executeNamed conn insertNote note *>
                 executeNamed conn insertCard card
               , i + 2)
    in fst . foldr reducer (pure (), j)

type VerbConfig = (Bool, Bool)

--This should just really be the definition of toVerb
toVerb' :: FullWord -> Either String Verb
toVerb' = withLeft "word is not a verb!" . toVerb

parseVerb :: String -> Either String Verb
parseVerb = (fmap fst . P.runParser LP.wordOnly) >=> toVerb'

--This should just really be the definition of conjugate
conjugate' :: (VerbConfig, Verb) -> SimpleTense -> Either String FullWord
conjugate' =
    fmap (fmap (withLeft "could not conjugate")) (uncurry (uncurry conjugate))

conjugate'' :: (VerbConfig, String) -> SimpleTense -> Either String FullWord
conjugate'' fwv st = swap (fmap parseVerb fwv) >>= flip conjugate' st

conjugate''' :: (VerbConfig, String) -> SimpleTense -> Either String CardData
conjugate''' (vc, str) st =
    fmap (conjugateToCardData str st . render) (conjugate'' (vc, str) st)

allTenses :: [SimpleTense]
allTenses =
    [ Conditional
    , Future
    , Imperfect
    , Present
    , Preterite
    , PresentSubjunctive
    , ImperfectSubjunctive
    ] <*>
    [Yo, Tú, Usted, Nosotros, Ustedes]

configuredVerbToCardDatas :: (VerbConfig, String) -> Either String [CardData]
configuredVerbToCardDatas stringVerb =
    swap (fmap (conjugate''' stringVerb) allTenses)

mkCardDatas :: [(VerbConfig, String)] -> Either String [CardData]
mkCardDatas = foldr (liftA2 (++) . configuredVerbToCardDatas) (Right [])

verbs :: [(VerbConfig, String)]
verbs =
    [ ((True, False), "avergonzar")
    , ((False, True), "pedir")
    , ((False, False), "correr")
    , ((True, True), "dormir")
    ]

main :: IO ()
main =
    case mkCardDatas verbs of
        Left err -> error err
        Right cardDatas -> do
            conn <- open "test.db"
            execute_ conn createCol
            execute_ conn initCol
            execute_ conn createNotes
            execute_ conn createCards
            execute_ conn createRevLog
            execute_ conn createGraves
            insertCards conn 1398130088495 cardDatas
            execute_ conn analyzeAndIndex
            close conn
