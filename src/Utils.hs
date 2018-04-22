{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Utils
    ( ($>)
    , liftA4
    , swapTuple
    , mHead
    , withLeft
    , swap
    , ifAppA
    , maybeUnit
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

ifAppA :: Applicative a => (b -> a b) -> Bool -> b -> a b
ifAppA f bool =
    if bool
        then f
        else pure

swapTuple :: (a, (b, c)) -> (b, (a, c))
swapTuple (a, (b, c)) = (b, (a, c))

mHead :: [a] -> Maybe a
mHead (h:_) = Just h
mHead _ = Nothing

withLeft :: a -> Maybe b -> Either a b
withLeft l m =
    case m of
        Just x -> Right x
        Nothing -> Left l

maybeUnit :: Bool -> Maybe ()
maybeUnit b =
    if b
        then Just ()
        else Nothing

class Swap a b c where
    swap :: a (b c) -> b (a c)

instance Swap ((,) a) Maybe b where
    swap (x, my) = fmap ((,) x) my

instance Swap ((,) a) (Either b) c where
    swap (x, ey) = fmap ((,) x) ey

instance Monoid a => Swap [] (Either a) b where
    swap =
        let go [] built = built
            go (h:t) built =
                case h of
                    Left x ->
                        go
                            t
                            (case built of
                                 Left y -> Left (x `mappend` y)
                                 Right _ -> Left x)
                    Right x -> go t (fmap ((:) x) built)
        in flip go (Right [])

instance Swap Maybe (Either a) b where
    swap m =
        case m of
            Just e ->
                case e of
                    Left x -> Left x
                    Right x -> Right (Just x)
            Nothing -> Right Nothing
