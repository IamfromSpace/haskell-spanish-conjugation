{-# LANGUAGE LambdaCase, RankNTypes #-}

module Linguistics.Intermediate
    ( toIntermediate
    , fromIntermediate
    , _jointCore
    , _penultimateCore
    , couldDiphthongize
    , couldVowelRaise
    , preventStressedJointDiphthongization
    , preventAmbiguiousJointDiphthongization
    , preventUirNonIDiphthongization
    , breakDiphthong
    , couldCToZc
    , cToZc
    , couldYoGo
    , yoGo
    , couldShortenedInfinitives
    , shortenedInfinitives
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe, isNothing, maybe)
import qualified Data.Maybe as Maybe
import Linguistics.Positional (startsWith)
import Linguistics.Stress
import Linguistics.Types
import Utils (($>), mHead)

-- This is all pretty ugly and shows that these types are hardly
-- "transparent" to the user.  All these actions should be performed
-- through helper methods rather than destructuring and restructuring.
toIntermediate :: Verb -> Ending -> Intermediate
toIntermediate (vt, mo, coreClusters_0, mhv1) ending =
    let ((isStressed, (mhv2, e)), iss, coda) = ending
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
    in (vt, mo, coreClusters1, ((isStressed, (mhv, e)), iss, coda))

joinStemList :: Core -> [(Core, InnerCluster)] -> (Core, [InnerSyllable])
joinStemList =
    let go built core [] = (core, built)
        go built core0 ((core1, cluster):iss) =
            go ((cluster, core0) : built) core1 iss
    in go []

fromIntermediate :: Intermediate -> FullWord
fromIntermediate (_, mo, stemList, (core, endingList, coda)) =
    let (c, slist) = joinStemList core stemList
    in (mo, c, slist ++ endingList, coda)

_jointCore :: Lens' Intermediate Core
_jointCore = _4 . _1

_endingSyllables :: Lens' Intermediate [InnerSyllable]
_endingSyllables = _4 . _2

_nextCore :: Traversal' Intermediate Core
_nextCore = _endingSyllables . _head . _2

_penultimateCore :: Traversal' Intermediate Core
_penultimateCore = _3 . _head . _1

_penultimateInnerCluster :: Traversal' Intermediate InnerCluster
_penultimateInnerCluster = _3 . _head . _2

getImplicitStressJointRelativeOffset :: Intermediate -> Int
getImplicitStressJointRelativeOffset (_, _, _, (_, e, maybeCoda)) =
    let penultimate =
            case maybeCoda of
                Just (Coda andS c) -> andS || c == Regular N || c == Regular S
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

couldVowelRaise :: Intermediate -> Bool
couldVowelRaise intermediate@(_, _, _, ending@(joint, _, _)) =
    let jointIsImplicitlyStressed =
            getImplicitStressJointRelativeOffset intermediate == 0
        jointHasStressedI =
            hasStressableI joint &&
            (hasExplicitStress joint || jointIsImplicitlyStressed)
    in not (startsWithIR ending || jointHasStressedI)

{- TODO: These three rules could use some work on approach/implementation...
 the pattern match is crazy, but the alternative is about a million checks. -}
-- Idea here is to prevent a stressed 'i' from becoming a semi-vowel in another core.
-- An example is "caer" + "imos" == "caímos" (not "caimos").  Without this rule,
-- the 'i' becomes a semi-vowel, and the 'a' would get the stress,
-- even though it should fall on the 'i'.
preventStressedJointDiphthongization :: Intermediate -> Intermediate
preventStressedJointDiphthongization intermediate@(vt, mOnset, iss@((_, Nothing):_), ((False, x@(Nothing, Left I)), y, z)) =
    if getImplicitStressJointRelativeOffset intermediate == 0
        then (vt, mOnset, iss, ((True, x), y, z))
        else intermediate
preventStressedJointDiphthongization x = x

-- This rule itself seems logically unnecessary, in that syllables are right biased
-- and as such, "aiaeai" is parsable as "a.ia'e.ai", however, it does _look_ confusing
-- as to which side the first 'i' should dipthongize with.  As such, it is changed
-- to a 'y' (which has basically the same sound), to allow the consonant rule make
-- it more evident which syllable it will be part of.
-- Notably, this rule is absent in Portuguese.
preventAmbiguiousJointDiphthongization :: Intermediate -> Intermediate
preventAmbiguiousJointDiphthongization (vt, a, (b, Nothing):c, ((d, (Just I, e)), f, g)) =
    ( vt
    , a
    , (b, Just (Nothing, Single (Regular Y))) : c
    , ((d, (Nothing, e)), f, g))
preventAmbiguiousJointDiphthongization x = x

-- This is yet another y rule.  If we ever end up with a joint that is /u[^i]/
-- we need to promote the u into the stem and with a medial y.
-- so construir + o == construyo
preventUirNonIDiphthongization :: Intermediate -> Intermediate
preventUirNonIDiphthongization intermediate@(vt@IR, mOnset, iss, ((isAccented, (Just U, v)), a, b)) =
    case v of
        Left I -> intermediate
        _ ->
            ( vt
            , mOnset
            , ((False, (Nothing, Left U)), Just (Nothing, Single (Regular Y))) :
              iss
            , ((isAccented, (Nothing, v)), a, b))
preventUirNonIDiphthongization x = x

-- Still, uh, getting the hang of lenses here.
-- Probably look back on this and think this is silly...
-- Use of _3 and the raw construction of a Core here
-- Says I'm still far too coupled to the data structures
breakDiphthong :: Intermediate -> Maybe Intermediate
breakDiphthong intermediate =
    if not (hasExplicitStress intermediate) &&
       getImplicitStressJointRelativeOffset intermediate == -1
        then case view (_jointCore . _semiVowelLeft) intermediate of
                 Just x ->
                     Just $
                     (over _3 ((:) ((True, (Nothing, Left x)), Nothing)) .
                      set (_jointCore . _semiVowelLeft) Nothing)
                         intermediate
                 Nothing -> do
                     penultimateCore <- preview _penultimateCore intermediate
                     -- The current penultimate core without the semivowel left
                     let newPenpenultimateCore =
                             set _semiVowelRight Nothing penultimateCore
                     -- which will be inserted as a new core+cluster after head
                     let insertNew =
                             over
                                 (_3 . _tail)
                                 ((:) (newPenpenultimateCore, Nothing))
                     -- The semiVowelRight gets upgraded and overwrites the current head
                     semiVowelToUpgrade <-
                         preview (_semiVowelRight . _Just) penultimateCore
                     let newPenultimateCore =
                             (True, (Nothing, Left semiVowelToUpgrade))
                     let updateHead = set (_3 . _head . _1) newPenultimateCore
                     -- Perform the updates and wrap them back up
                     return ((insertNew . updateHead) intermediate)
        else Just intermediate

couldCToZc :: Intermediate -> Bool
couldCToZc (_, _, _, ending) = startsWith A ending || startsWith O ending

cToZc :: Intermediate -> Maybe Intermediate
cToZc intermediate =
    let c = Just (Nothing, Single (Regular SoftC))
        zc = Just (Just (Coda False (Regular SoftC)), Single (StopOrF HardC))
    in preview _penultimateInnerCluster intermediate >>=
       (\cluster ->
            if cluster == c
                then return (set _penultimateInnerCluster zc intermediate)
                else Nothing)

couldYoGo :: Intermediate -> Bool
couldYoGo = couldCToZc

yoGo :: Intermediate -> Maybe Intermediate
yoGo intermediate =
    let pushG x =
            set
                _penultimateInnerCluster
                (Just (Just (Coda False x), Single (StopOrF HardG)))
                intermediate
        setG =
            set
                _penultimateInnerCluster
                (Just (Nothing, Single (StopOrF HardG)))
                intermediate
    in preview _penultimateInnerCluster intermediate >>=
       (\case
            Just (Nothing, Single x@(Regular N)) -> Just (pushG x)
            Just (Nothing, Single x@(Regular S)) -> Just (pushG x)
            Just (Nothing, Single x@(Liquid L)) -> Just (pushG x)
            Just (Nothing, Single (Regular SoftC)) -> Just setG
            Nothing ->
                preview (_penultimateCore . _semiVowelRight) intermediate $>
                set (_penultimateCore . _semiVowelRight) (Just I) setG
            _ -> Nothing)

couldShortenedInfinitives :: Intermediate -> Bool
couldShortenedInfinitives intermediate
    -- no diphthongs in the joint (ex. must not apply to -ieron)
    -- and the first ending consonant cluster starts with 'r'
    -- (which is _not_ in the coda, meaning it's not the infinitive itself)
 =
    isNothing (view (_jointCore . _semiVowelLeft) intermediate) &&
    maybe True isNothing (preview (_jointCore . _semiVowelRight) intermediate) &&
    maybe
        False
        (startsWith R)
        (preview (_endingSyllables . _head . _1) intermediate)

shortenedInfinitives :: Intermediate -> Maybe Intermediate
shortenedInfinitives intermediate =
    let popSyllable ::
               Lens' Intermediate [a] -> Intermediate -> Maybe Intermediate
        popSyllable listLens x
         -- Remove the first (non-joint) syllable
         -- from the lens'd (consonants + core)
         = fmap (flip (set listLens) x) (preview (listLens . _tail) x)
        popEndingSyllable = popSyllable _endingSyllables
        popStemSyllable = popSyllable _3
        moveACoreToJoint ::
               Traversal' Intermediate Core
            -> Intermediate
            -> Maybe Intermediate
        moveACoreToJoint coreLens x
         -- Take a lens'd core, and set the joint to that value
         = fmap (flip (set _jointCore) x) (preview coreLens x)
        moveNextCoreToJoint = moveACoreToJoint _nextCore
        movePenultimateCoreToJoint = moveACoreToJoint _penultimateCore
        popEndingAndPullItsCoreToJoint
         -- drop next ending syllable, but replace the joint with its core
         = moveNextCoreToJoint >=> popEndingSyllable
        addLiquidForDouble stopOrF =
            set (_penultimateInnerCluster . _Just . _2) (Double stopOrF R)
        insertRIntoCoda =
            set
                (_penultimateInnerCluster . _Just . _1)
                (Just (Coda False (Liquid R)))
        insertConsonantIntoCodaAndDrIntoOnset consonant =
            set
                (_penultimateInnerCluster . _Just)
                (Just (Coda False consonant), Double D R)
    in preview _penultimateInnerCluster intermediate >>=
         -- The first two rules are pretty strict. Theoretically, this could
         -- work with any StopOrF, and a Coda _could_ be defined as well.
         -- However, I can't find any example words that behave that way,
         -- and I've consistently erred on the side of strictness.
       (\case
            Just (Nothing, Single (StopOrF x@B)) ->
                popEndingAndPullItsCoreToJoint $
                addLiquidForDouble x intermediate
            Just (Nothing, Single (StopOrF x@D)) ->
                popEndingAndPullItsCoreToJoint $
                addLiquidForDouble x intermediate
            Just (Nothing, Single (Liquid R)) ->
                popEndingAndPullItsCoreToJoint $ insertRIntoCoda intermediate
            Just (Nothing, Single x@(Liquid L)) ->
                popEndingAndPullItsCoreToJoint $
                insertConsonantIntoCodaAndDrIntoOnset x intermediate
            Just (Nothing, Single x@(Regular N)) ->
                popEndingAndPullItsCoreToJoint $
                insertConsonantIntoCodaAndDrIntoOnset x intermediate
            Just (_, Single (Regular SoftC))
            -- drop -ec the -ce (or fail)
             ->
                case view _jointCore intermediate of
                    (_, (_, Right (E, _))) ->
                        movePenultimateCoreToJoint intermediate >>=
                        popStemSyllable
                    _ ->
                        case preview _penultimateCore intermediate of
                            Just (_, (_, Right (E, _))) ->
                                popStemSyllable intermediate
                            _ -> Nothing
            _ -> Nothing)
