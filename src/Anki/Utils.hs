module Anki.Utils
    ( getAnkiCheckSum
    ) where

import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord)

getAnkiCheckSum :: String -> Int
getAnkiCheckSum s
    --Sha1 hash >>> first 4 bytes >>> as an Int
 = BS.foldl (\p n -> 256 * p + ord n) 0 (BS.take 4 (hash (BS.pack s)))
