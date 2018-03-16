module Utils
    ( ($>)
    , liftA4
    ) where

($>) :: Functor f => f a -> b -> f b
($>) = flip (fmap . const)

liftA4 ::
       Applicative a
    => (b -> c -> d -> e -> f)
    -> a b
    -> a c
    -> a d
    -> a e
    -> a f
liftA4 f a0 a1 a2 a3 = f <$> a0 <*> a1 <*> a2 <*> a3
