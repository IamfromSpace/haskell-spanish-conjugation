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

type Core
     = Accentable ( Maybe HighVowel
                  , Either HighVowel (LowVowel, Maybe HighVowel))

data Liquid
    = R
    | L
    deriving (Show, Eq)

data StopOrF
    = B
    | D
    | F
    | G GCreator
    | K KCreator
    | P
    | T
    deriving (Show, Eq)

data GCreator
    = GFromG
    | GFromGU
    deriving (Show, Eq)

data KCreator
    = KFromQU
    | KFromC
    | KFromK
    deriving (Show, Eq)

data Regular
    = CH
    | M
    | N
    | Ñ
    | Y
    | S SCreator
    | V
    | X XCreator
    deriving (Show, Eq)

data SCreator
    = SFromZ
    | SFromC
    | SFromS
    | SFromX
    deriving (Show, Eq)

data XCreator
    = XFromJ
    | XFromG
    deriving (Show, Eq)

data Consonant
    = Liquid Liquid
    | StopOrF StopOrF
    | Regular Regular
    deriving (Show, Eq)

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
