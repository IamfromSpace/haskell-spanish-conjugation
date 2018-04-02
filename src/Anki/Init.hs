{-# LANGUAGE OverloadedStrings #-}

module Anki.Init
    ( createTables
    , analyzeAndIndex
    ) where

import Database.SQLite.Simple (Query, execute_)
import Database.SQLite.Simple.Internal (Connection)

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

createTables :: Connection -> IO ()
createTables conn =
    let e = execute_ conn
    in e createCol *> e createNotes *> e createCards *> e createRevLog *>
       e createGraves

analyze :: Query
analyze = "ANALYZE sqlite_master;"

addSqliteStat :: Query
addSqliteStat = "INSERT INTO \"sqlite_stat1\" VALUES('col',NULL,'1');"

addNotesUsnIndex :: Query
addNotesUsnIndex = "CREATE INDEX ix_notes_usn on notes (usn);"

addCardsUsnIndex :: Query
addCardsUsnIndex = "CREATE INDEX ix_cards_usn on cards (usn);"

addRevlogUsnIndex :: Query
addRevlogUsnIndex = "CREATE INDEX ix_revlog_usn on revlog (usn);"

addCardsNidIndex :: Query
addCardsNidIndex = "CREATE INDEX ix_cards_nid on cards (nid);"

addCardDidQueueDueIndex :: Query
addCardDidQueueDueIndex =
    "CREATE INDEX ix_cards_sched on cards (did, queue, due);"

addRevlogCidIndex :: Query
addRevlogCidIndex = "CREATE INDEX ix_revlog_cid on revlog (cid);"

addNotesCsumIndex :: Query
addNotesCsumIndex = "CREATE INDEX ix_notes_csum on notes (csum);"

analyzeAndIndex :: Connection -> IO ()
analyzeAndIndex conn =
    let e = execute_ conn
    in e analyze *> e addSqliteStat *> e addNotesUsnIndex *> e addCardsUsnIndex *>
       e addRevlogUsnIndex *>
       e addCardsNidIndex *>
       e addCardDidQueueDueIndex *>
       e addRevlogCidIndex *>
       e addNotesCsumIndex
