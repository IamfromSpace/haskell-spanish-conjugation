module Linguistics.Intermediate
    ( toStem
    , toIntermediate
    , fromIntermediate
    , couldDiphthongize
    , couldVowelRaise
    , preventStressedJointDiphthongization
    , preventAmbiguiousJointDiphthongization
    ) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Linguistics.Positional (startsWith)
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
        -- the first syllable of the ending have a colliding left semi-vowel
        -- (which must be a u/i), the we add another syllable, where the u gets
        -- upgraded to the vowel, and the i becomes a y in the onset.
        -- This is rare, and notably only happens in -uir verbs like
        -- construir + iendo
        --       ^     ^
        -- where the indicated u and i _both_ want to be the left semi-vowel
        -- in the core.
                then ( Nothing
                     , ( (False, (Nothing, Left U))
                       , Just (Nothing, Single (Regular Y))) :
                       coreClusters_0)
                else (mhv1 <|> mhv2, coreClusters_0)
    in ((mo, coreClusters1), ((isStressed, (mhv, e)), iss, coda))

joinStemList :: Core -> [(Core, InnerCluster)] -> (Core, [InnerSyllable])
joinStemList =
    let go built core [] = (core, built)
        go built core0 ((core1, cluster):iss) =
            go ((cluster, core0) : built) core1 iss
    in go []

fromIntermediate :: Intermediate -> FullWord
fromIntermediate ((mo, stemList), (core, endingList, coda)) =
    let (c, slist) = joinStemList core stemList
    in (mo, c, slist ++ endingList, coda)

getImplicitStressJointRelativeOffset :: Intermediate -> Int
getImplicitStressJointRelativeOffset (_, (_, e, maybeCoda)) =
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

hasStressableI :: Core -> Bool
hasStressableI (_, (_, Left I)) = True
hasStressableI _ = False

startsWithIR :: Ending -> Bool
startsWithIR (jointCore, innerSyllables, mCoda) =
    hasStressableI jointCore &&
    fromMaybe
        False
        (fmap (startsWith R . fst) (mHead innerSyllables) <|>
         fmap (startsWith R) mCoda)

couldVowelRaise :: Bool -> Intermediate -> Bool
couldVowelRaise diphthongizes intermediate@(_, ending@(joint, _, _)) =
    let willDiphthongize = diphthongizes && couldDiphthongize intermediate
        jointIsImplicitlyStressed =
            getImplicitStressJointRelativeOffset intermediate == 0
        jointHasStressedI =
            hasStressableI joint &&
            (hasExplicitStress joint || jointIsImplicitlyStressed)
    in not willDiphthongize && not (startsWithIR ending || jointHasStressedI)

{- TODO: These two rules could use some work on approach/implementation...
 the pattern match is crazy, but the alternative is about a million checks. -}
-- Idea here is to prevent a stressed 'i' from becoming a semi-vowel in another core.
-- An example is "caer" + "imos" == "caímos" (not "caimos").  Without this rule,
-- the 'i' becomes a semi-vowel, and the 'a' would get the stress,
-- even though it should fall on the 'i'.
preventStressedJointDiphthongization :: Intermediate -> Intermediate
preventStressedJointDiphthongization intermediate@(stem@(_, (_, Nothing):_), ((False, x@(Nothing, Left I)), y, z)) =
    if getImplicitStressJointRelativeOffset intermediate == 0
        then (stem, ((True, x), y, z))
        else intermediate
preventStressedJointDiphthongization x = x

-- This rule itself seems logically unnecessary, in that syllables are right biased
-- and as such, "aiaeai" is parsable as "a.ia'e.ai", however, it does _look_ confusing
-- as to which side the first 'i' should dipthongize with.  As such, it is changed
-- to a 'y' (which has basically the same sound), to allow the consonant rule make
-- it more evident which syllable it will be part of.
-- Notably, this rule is absent in Portuguese.
preventAmbiguiousJointDiphthongization :: Intermediate -> Intermediate
preventAmbiguiousJointDiphthongization ((a, (b, Nothing):c), ((d, (Just I, e)), f, g)) =
    ( (a, (b, Just (Nothing, Single (Regular Y))) : c)
    , ((d, (Nothing, e)), f, g))
preventAmbiguiousJointDiphthongization x = x
