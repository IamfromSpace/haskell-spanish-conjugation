module Linguistics.Intermediate
    ( toStem
    , toIntermediate
    , fromIntermediate
    ) where

import Control.Applicative
import qualified Data.Maybe as Maybe
import Linguistics.Types

dropLastCore ::
       Core -> [InnerSyllable] -> (Maybe HighVowel, [(Core, InnerCluster)])
dropLastCore =
    let go built a [] = (fst a, built)
        go built a ((cluster, b):t) = go ((a, cluster) : built) b t
    in go []

toStem :: FullWord -> (Maybe HighVowel, Stem)
toStem (onset, core, innerSyllables, _) =
    let (mhv, l) = dropLastCore core innerSyllables
    in (mhv, (onset, l))

-- This is all pretty ugly and shows that these types are hardly
-- "transparent" to the user.  All these actions should be performed
-- through helper methods rather than destructuring and restructuring.
toIntermediate :: FullWord -> Ending -> Intermediate
toIntermediate infinitive ending =
    let (mhv1, (mo, coreClusters_0)) = toStem infinitive
        ((mhv2, e), iss, coda) = ending
        (mhv, coreClusters1) =
            if Maybe.isJust mhv1 && Maybe.isJust mhv2
        -- This ugliness means that if the final syllable of the stem and
        -- the first syllable of the ending have a colliding dipthong (which
        -- must be a u/i) we add another syllable, where the u gets upgraded
        -- to the vowel, and the i becomes a y in the onset.
                then ( Nothing
                     , ( (Nothing, Left (False, U))
                       , Just (Nothing, Single (Regular Y))) :
                       coreClusters_0)
                else (mhv1 <|> mhv2, coreClusters_0)
    in ((mo, coreClusters1), ((mhv, e), iss, coda))

joinStemList :: Core -> [(Core, InnerCluster)] -> (Core, [InnerSyllable])
joinStemList =
    let go built core [] = (core, built)
        go built core0 ((core1, cluster):tail) =
            go ((cluster, core0) : built) core1 tail
    in go []

fromIntermediate :: Intermediate -> FullWord
fromIntermediate ((mo, stemList), (core, endingList, coda)) =
    let (c, slist) = joinStemList core stemList
    in (mo, c, slist ++ endingList, coda)