{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Intermediate
    ( toStem
    , toIntermediate
    , fromIntermediate
    , couldDiphthongize
    , couldVowelRaise
    ) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Linguistics.Diphthongizing
import Linguistics.Stress
import Linguistics.Types
import Utils (mHead)

dropLastCore ::
       Core -> [InnerSyllable] -> (Maybe HighVowel, [(Core, InnerCluster)])
dropLastCore =
    let go built a [] = (fst (snd a), built)
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
        ((isStressed, (mhv2, e)), iss, coda) = ending
        (mhv, coreClusters1) =
            if Maybe.isJust mhv1 && Maybe.isJust mhv2
        -- This ugliness means that if the final syllable of the stem and
        -- the first syllable of the ending have a colliding dipthong (which
        -- must be a u/i) we add another syllable, where the u gets upgraded
        -- to the vowel, and the i becomes a y in the onset.
                then ( Nothing
                     , ( (False, (Nothing, Left U))
                       , Just (Nothing, Single (Regular Y))) :
                       coreClusters_0)
                else (mhv1 <|> mhv2, coreClusters_0)
    in ((mo, coreClusters1), ((isStressed, (mhv, e)), iss, coda))

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

getImplicitStressJointRelativeOffset :: Intermediate -> Int
getImplicitStressJointRelativeOffset ((_, s), (j, e, maybeCoda)) =
    let penultimate =
            case maybeCoda of
                Just (Coda andS c) ->
                    andS || c == Regular N || c == Regular (S SFromS)
                Nothing -> True
    in length e -
       if penultimate
           then 1
           else 0

couldDiphthongize :: Intermediate -> Bool
couldDiphthongize intermediate =
    getImplicitStressJointRelativeOffset intermediate == -1 &&
    not (hasExplicitStress intermediate)

-- TODO: This seems to be generalizable to a CanStartWith a b,
-- which would prevent the need to generate lots of type classes
-- for specific letters/sounds/etc.  However, this whole approach
-- does seem a little intractable in how it scales out.
class CanStartWithR a where
    startsWithR :: a -> Bool

instance CanStartWithR Coda where
    startsWithR (Coda _ (Liquid R)) = True
    startsWithR _ = False

instance CanStartWithR Onset where
    startsWithR (Single (Liquid R)) = True
    startsWithR _ = False

instance CanStartWithR InnerCluster where
    startsWithR (Just (Nothing, onset)) = startsWithR onset
    startsWithR (Just (Just coda, _)) = startsWithR coda
    startsWithR _ = False

hasStressableI :: Core -> Bool
hasStressableI (_, (_, Left I)) = True
hasStressableI _ = False

startsWithIR :: Ending -> Bool
startsWithIR (jointCore, innerSyllables, mCoda) =
    hasStressableI jointCore &&
    fromMaybe
        False
        (fmap (startsWithR . fst) (mHead innerSyllables) <|>
         fmap startsWithR mCoda)

couldVowelRaise :: Bool -> Intermediate -> Bool
couldVowelRaise diphthongizes intermediate@(_, ending@(joint, _, _)) =
    let willDiphthongize = diphthongizes && couldDiphthongize intermediate
        jointIsImplicitlyStressed =
            getImplicitStressJointRelativeOffset intermediate == 0
        jointHasStressedI =
            hasStressableI joint &&
            (hasExplicitStress joint || jointIsImplicitlyStressed)
    in not willDiphthongize && not (startsWithIR ending || jointHasStressedI)
