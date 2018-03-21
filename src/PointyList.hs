module PointyList
    ( PointyList(..)
    , toList
    , fromList
    ) where

import qualified Control.Arrow as Arrow

data PointyList b a =
    PointyList [(a, b)]
               a
               [(b, a)]

instance Functor (PointyList b) where
    fmap f (PointyList l m r) =
        PointyList (fmap (Arrow.first f) l) (f m) (fmap (fmap f) r)

toList :: PointyList b a -> (a, [(a, b)])
toList (PointyList l m r) =
    let go built m' [] = (m', built)
        go built m' ((r', r''):t) = go ((m', r') : built) r'' t
    in go l m r

fromList :: a -> [(b, a)] -> PointyList b a
fromList = PointyList []
