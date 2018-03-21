{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Linguistics.Render
    ( contextualRender
    , render
    ) where

import Linguistics.Positional (endsWith, startsWith)
import Linguistics.Types

-- TODO: should this be any type rather than bool?
class ContextualRender a where
    contextualRender :: Bool -> a -> String

instance ContextualRender GCreator where
    contextualRender True GFromG = "gu"
    contextualRender False GFromG = "g"
    contextualRender True GFromGU = "gu"
    contextualRender False GFromGU = "g"

instance ContextualRender KCreator where
    contextualRender True KFromQU = "qu"
    contextualRender False KFromQU = "c"
    contextualRender True KFromC = "qu"
    contextualRender False KFromC = "c"
    contextualRender _ KFromK = "k"

instance ContextualRender SCreator where
    contextualRender _ SFromZ = "z"
    contextualRender True SFromC = "c"
    contextualRender False SFromC = "z"
    contextualRender _ SFromS = "s"
    contextualRender _ SFromX = "x"

instance ContextualRender XCreator where
    contextualRender True XFromG = "g"
    contextualRender _ _ = "j"

instance ContextualRender Liquid where
    contextualRender _ R = "r"
    contextualRender _ L = "l"

instance ContextualRender StopOrF where
    contextualRender _ B = "b"
    contextualRender _ D = "d"
    contextualRender _ F = "f"
    contextualRender b (G gc) = contextualRender b gc
    contextualRender b (K kc) = contextualRender b kc
    contextualRender _ P = "p"
    contextualRender _ T = "t"

instance ContextualRender Regular where
    contextualRender _ CH = "ch"
    contextualRender _ H = "h"
    contextualRender _ M = "m"
    contextualRender _ N = "n"
    contextualRender _ Ñ = "ñ"
    contextualRender _ Y = "y"
    contextualRender b (S sc) = contextualRender b sc
    contextualRender _ V = "v"
    contextualRender b (X xc) = contextualRender b xc

instance ContextualRender Consonant where
    contextualRender b (Liquid x) = contextualRender b x
    contextualRender b (StopOrF x) = contextualRender b x
    contextualRender b (Regular x) = contextualRender b x

instance ContextualRender Onset where
    contextualRender b (Single c) = contextualRender b c
    contextualRender b (Double s l) =
        contextualRender False s ++ contextualRender b l

instance ContextualRender InnerCluster where
    contextualRender _ Nothing = ""
    contextualRender b (Just (mc, o)) = render mc ++ contextualRender b o

instance ContextualRender HighVowel
    -- Boolean means "after G-Sound and before I/E"
                                                    where
    contextualRender True U = "ü"
    contextualRender False U = "u"
    contextualRender _ sv = render (False, sv)

-- TODO: Whew, can this be less ugly?
instance ContextualRender Core
    -- Boolean means "after G-Sound"
                                     where
    contextualRender _ (b, (Nothing, Left v)) = render (b, v)
    contextualRender _ (b, (Nothing, Right (v, svr))) =
        render (b, v) ++ render (fmap ((,) False) svr)
    contextualRender False (b, (Just svl, Left v)) =
        contextualRender False svl ++ render (b, v)
    contextualRender True (b, (Just svl, Left I)) =
        contextualRender True svl ++ render (b, I)
    contextualRender _ (b, (Just svl, Left U)) =
        contextualRender False svl ++ render (b, U)
    contextualRender False (b, (Just svl, Right (v, svr))) =
        contextualRender False svl ++
        render (b, v) ++ render (fmap ((,) False) svr)
    contextualRender True (b, (Just svl, Right (E, svr))) =
        contextualRender True svl ++
        render (b, E) ++ render (fmap ((,) False) svr)
    contextualRender _ (b, (Just svl, Right (v, svr))) =
        contextualRender False svl ++
        render (b, v) ++ render (fmap ((,) False) svr)

class Render a where
    render :: a -> String

instance Render a => Render (Maybe a) where
    render (Just x) = render x
    render Nothing = ""

instance Render a => Render [a] where
    render = concatMap render

instance Render Coda
  -- A Coda can never preceed a e or i, because it must be the end of the word
  -- or be immediately followed by an onset
                                            where
    render (Coda False c) = contextualRender False c
    render (Coda True c) = contextualRender False c ++ "s"

instance Render (Accentable HighVowel) where
    render (True, I) = "í"
    render (False, I) = "i"
    render (True, U) = "ú"
    render (False, U) = "u"

instance Render (Accentable LowVowel) where
    render (True, A) = "á"
    render (False, A) = "a"
    render (True, E) = "é"
    render (False, E) = "e"
    render (True, O) = "ó"
    render (False, O) = "o"

instance Render InnerSyllable where
    render (innerCluster, core) =
        contextualRender (startsWith I core || startsWith E core) innerCluster ++
        -- Notably, this doesn't actually check if it's a GFromG, just that it's a G
        contextualRender (endsWith (G GFromG) innerCluster) core

instance Render FullWord where
    render (Nothing, core, innerSyllables, mCoda) =
        contextualRender False core ++ render innerSyllables ++ render mCoda
    render (Just onset, core, innerSyllables, mCoda) =
        contextualRender (startsWith I core || startsWith E core) onset ++
        -- Notably, this doesn't actually check if it's a GFromG, just that it's a G
        contextualRender (endsWith (G GFromG) onset) core ++
        render innerSyllables ++ render mCoda