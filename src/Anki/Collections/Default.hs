{-# LANGUAGE OverloadedStrings #-}

module Anki.Collections.Default
    ( initCol
    , insertNote
    , insertCard
    , getNamedParamTuple
    , insertCards
    , CardData
    ) where

import Anki.Utils (getAnkiCheckSum)
import Database.SQLite.Simple
       (NamedParam((:=)), Query, executeNamed)
import Database.SQLite.Simple.Internal (Connection)

type CardData = (String, String, String, [String])

initCol :: Query
initCol =
    "INSERT INTO col VALUES(1,1332961200,1398130163295,1398130163168,11,0,0,0,'{\"nextPos\": 1, \"estTimes\": true, \"activeDecks\": [1], \"sortType\": \"noteFld\", \"timeLim\": 0, \"sortBackwards\": false, \"addToCur\": true, \"curDeck\": 1, \"newBury\": true, \"newSpread\": 0, \"dueCounts\": true, \"curModel\": \"1398130163168\", \"collapseTime\": 1200}','{\"1342697561419\": {\"vers\": [], \"name\": \"Basic\", \"tags\": [], \"did\": 1398130078204, \"usn\": -1, \"req\": [[0, \"all\", [0]]], \"flds\": [{\"name\": \"Front\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 0, \"font\": \"Arial\", \"size\": 12}, {\"name\": \"Back\", \"rtl\": false, \"sticky\": false, \"media\": [], \"ord\": 1, \"font\": \"Arial\", \"size\": 12}], \"sortf\": 0, \"latexPre\": \"\\\\documentclass[12pt]{article}\\n\\\\special{papersize=3in,5in}\\n\\\\usepackage{amssymb,amsmath}\\n\\\\pagestyle{empty}\\n\\\\setlength{\\\\parindent}{0in}\\n\\\\begin{document}\\n\", \"tmpls\": [{\"name\": \"Forward\", \"qfmt\": \"{{Front}}\", \"did\": null, \"bafmt\": \"\", \"afmt\": \"{{FrontSide}}\\n\\n\\n<hr id=answer/>\\n\\n{{Back}}\", \"ord\": 0, \"bqfmt\": \"\"}], \"latexPost\": \"\\\\end{document}\", \"type\": 0, \"id\": 1342697561419, \"css\": \".card {\\n font-family: arial;\\n font-size: 30px;\\n text-align: center;\\n color: black;\\n background-color: white;\\n}\\n\\n.card1 { background-color: #FFFFFF; }\", \"mod\": 1398130117}}','{\"1\": {\"desc\": \"\", \"name\": \"Default\", \"extendRev\": 50, \"usn\": 0, \"collapsed\": false, \"newToday\": [0, 0], \"timeToday\": [0, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [0, 0], \"lrnToday\": [0, 0], \"id\": 1, \"mod\": 1398130160}, \"1398130078204\": {\"desc\": \"\", \"name\": \"Spanish Haskell Conjugation\", \"extendRev\": 50, \"usn\": -1, \"collapsed\": false, \"newToday\": [754, 0], \"timeToday\": [754, 0], \"dyn\": 0, \"extendNew\": 10, \"conf\": 1, \"revToday\": [754, 0], \"lrnToday\": [754, 0], \"id\": 1398130078204, \"mod\": 1398130140}}','{\"1\": {\"name\": \"Default\", \"replayq\": true, \"lapse\": {\"leechFails\": 8, \"minInt\": 1, \"delays\": [10], \"leechAction\": 0, \"mult\": 0}, \"rev\": {\"perDay\": 100, \"fuzz\": 0.05, \"ivlFct\": 1, \"maxIvl\": 36500, \"ease4\": 1.3, \"bury\": true, \"minSpace\": 1}, \"timer\": 0, \"maxTaken\": 60, \"usn\": 0, \"new\": {\"perDay\": 20, \"delays\": [1, 10], \"separate\": true, \"ints\": [1, 4, 7], \"initialFactor\": 2500, \"bury\": true, \"order\": 1}, \"mod\": 0, \"id\": 1, \"autoplay\": true}}','{}');\n"

insertNote :: Query
insertNote =
    "INSERT INTO notes VALUES(:noteId,:noteGuid,1342697561419,:noteId,-1,:tags,:content,:searchValue,:checkSum,0,'');"

insertCard :: Query
insertCard =
    "INSERT INTO cards VALUES(:cardId,:noteId,1398130078204,0,:cardId,-1,0,0,:noteId,0,0,0,0,0,0,0,0,'');"

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
