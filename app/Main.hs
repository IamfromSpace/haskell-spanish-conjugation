{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (pure)
import Database.SQLite.Simple
       (NamedParam((:=)), Query, close, execute, executeNamed, execute_,
        open)
import qualified Linguistics.Parsers as LP
import qualified Parser as P

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
    "INSERT INTO col VALUES(1,1332961200,1398130163295,1398130163168,11,0,0,0,'{\"nextPos\": 1, \"estTimes\": true, \"activeDecks\": [1], \"sortType\": \"noteFld\", \"timeLim\": 0, \"sortBackwards\": false, \"addToCur\": true, \"curDeck\": 1, \"newBury\": true, \"newSpread\": 0, \"dueCounts\": true, \"curModel\": \"1398130163168\", \"collapseTime\": 1200}','{\"1342697561419\": {\"vers\": [], \"name\": \"Basic\", \"tags\": [], \"did\": 1398130078204, \"usn\": -1, \"req\": [[0, \"all\", [0]]], \"flds\": [{\"name\": \"Front\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 0, \"font\": \"Arial\", \"size\": 12}, {\"name\": \"Back\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 1, \"font\": \"Arial\", \"size\": 12}], \"sortf\": 0, \"latexPre\": \"\\\\documentclass[12pt]{article}\\n\\\\special{papersize=3in,5in}\\n\\\\usepackage{amssymb,amsmath}\\n\\\\pagestyle{empty}\\n\\\\setlength{\\\\parindent}{0in}\\n\\\\begin{document}\\n\", \"tmpls\": [{\"name\": \"Forward\", \"qfmt\": \"{{Front}}\", \"did\": null, \"bafmt\": \"\", \"afmt\": \"{{FrontSide}}\\n\\n\\n<hr id=answer/>\\n\\n{{Back}}\", \"ord\": 0, \"bqfmt\": \"\"}], \"latexPost\": \"\\\\end{document}\", \"type\": 0, \"id\": 1342697561419, \"css\": \".card {\\n font-family: arial;\\n font-size: 30px;\\n text-align: center;\\n color: black;\\n background-color: white;\\n}\\n\\n.card1 { background-color: #FFFFFF; }\", \"mod\": 1398130117}}','{\"1\": {\"desc\": \"\", \"name\": \"Default\", \"extendRev\": 50, \"usn\": 0, \"collapsed\": false, \"newToday\": [0, 0], \"timeToday\": [0, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [0, 0], \"lrnToday\": [0, 0], \"id\": 1, \"mod\": 1398130160}, \"1398130078204\": {\"desc\": \"\", \"name\": \"tatoeba\", \"extendRev\": 50, \"usn\": -1, \"collapsed\": false, \"newToday\": [754, 0], \"timeToday\": [754, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [754, 0], \"lrnToday\": [754, 0], \"id\": 1398130078204, \"mod\": 1398130140}}','{\"1\": {\"name\": \"Default\", \"replayq\": true, \"lapse\": {\"leechFails\": 8, \"minInt\": 1, \"delays\": [10], \"leechAction\": 0, \"mult\": 0}, \"rev\": {\"perDay\": 100, \"fuzz\": 0.05, \"ivlFct\": 1, \"maxIvl\": 36500, \"ease4\": 1.3, \"bury\": true, \"minSpace\": 1}, \"timer\": 0, \"maxTaken\": 60, \"usn\": 0, \"new\": {\"perDay\": 20, \"delays\": [1, 10], \"separate\": true, \"ints\": [1, 4, 7], \"initialFactor\": 2500, \"bury\": true, \"order\": 1}, \"mod\": 0, \"id\": 1, \"autoplay\": true}}','{}');\n"

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

main :: IO ()
main = do
    conn <- open "test.db"
    execute_ conn createCol
    execute_ conn initCol
    execute_ conn createNotes
    execute_ conn createCards
    execute_ conn createRevLog
    execute_ conn createGraves
    executeNamed
        conn
        insertNote
        [ ":content" := ("Does it work?\US'It does!" :: String)
        , ":searchValue" := ("Does it work?" :: String)
        , ":tags" := ("space separated list" :: String)
        , ":checkSum" := (4077833205 :: Int)
        , ":noteGuid" := ("Ot0!xywPWG" :: String)
        , ":noteId" := (1398130088495 :: Int)
        ]
    executeNamed
        conn
        insertCard
        [ ":cardId" := (1398130088496 :: Int)
        , ":noteId" := (1398130088495 :: Int)
        ]
    execute_ conn analyzeAndIndex
    close conn
