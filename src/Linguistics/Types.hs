module Linguistics.Types
    ( HighVowel(..)
    , LowVowel(..)
    , Accentable
    , Core
    , Liquid(..)
    , StopOrF(..)
    , GCreator(..)
    , KCreator(..)
    , Regular(..)
    , SCreator(..)
    , XCreator(..)
    , Consonant(..)
    , Onset(..)
    , Coda(..)
    , Syllable(..)
    , InnerCluster
    , InnerSyllable
    , FullWord
    , Stem
    , Ending
    , Intermediate
    ) where

data HighVowel
    = U
    | I
    deriving (Show, Eq)

data LowVowel
    = A
    | E
    | O
    deriving (Show)

type Accentable a = (Bool, a)

accent :: Accentable a -> Accentable a
accent (_, a) = (True, a)

unaccent :: Accentable a -> Accentable a
unaccent (_, a) = (False, a)

type Core
     = ( Maybe HighVowel
       , Either (Accentable HighVowel) (Accentable LowVowel, Maybe HighVowel))

data Liquid
    = R
    | L
    deriving (Show)

data StopOrF
    = B
    | D
    | F
    | G GCreator
    | K KCreator
    | P
    | T
    deriving (Show)

data GCreator
    = GFromG
    | GFromGU
    deriving (Show)

data KCreator
    = KFromQU
    | KFromC
    | KFromK
    deriving (Show)

data Regular
    = CH
    | M
    | N
    | Ñ
    | Y
    | S SCreator
    | V
    | X XCreator
    deriving (Show)

data SCreator
    = SFromZ
    | SFromC
    | SFromS
    | SFromX
    deriving (Show)

data XCreator
    = XFromJ
    | XFromG
    deriving (Show)

data Consonant
    = Liquid Liquid
    | StopOrF StopOrF
    | Regular Regular
    deriving (Show)

data Onset
    = Single Consonant
    | Double StopOrF
             Liquid
    deriving (Show)

data Coda =
    Coda Bool
         Consonant
    deriving (Show)

data Syllable
    = CoreOnly Core
    | OnsetAndCore Onset
                   Core
    | CoreAndCoda Core
                  Coda
    | OnsetCoreAndCoda Onset
                       Core
                       Coda
    deriving (Show)

type InnerCluster = Maybe (Maybe Coda, Onset)

type InnerSyllable = (InnerCluster, Core)

type FullWord = (Maybe Onset, Core, [InnerSyllable], Maybe Coda)

type Stem = (Maybe Onset, [(Core, InnerCluster)])

type Ending = (Core, [InnerSyllable], Maybe Coda)

type Intermediate = (Stem, Ending)
