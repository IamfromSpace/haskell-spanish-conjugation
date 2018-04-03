module Linguistics.Types
    ( HighVowel(..)
    , LowVowel(..)
    , Accentable
    , Core
    , Liquid(..)
    , StopOrF(..)
    , Regular(..)
    , Consonant(..)
    , Onset(..)
    , Coda(..)
    , VerbType(..)
    , InnerCluster
    , InnerSyllable
    , FullWord
    , Verb
    , Ending
    , Intermediate
    , Subject(..)
    , SubjectlessTense(..)
    , SubjectSensativeTense(..)
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
    | HardG
    | K
    | HardC
    | P
    | T
    deriving (Show, Eq)

data Regular
    = CH
    -- TODO: H can also appear on the core, and will evenutally
    -- need to be moved to its own data type.
    | SoftC
    | SoftG
    | H
    | J
    | M
    | N
    | Ñ
    | Y
    | S
    | V
    | X
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

data VerbType
    = AR
    | ER
    | IR
    deriving (Show, Eq)

type InnerCluster = Maybe (Maybe Coda, Onset)

type InnerSyllable = (InnerCluster, Core)

type FullWord = (Maybe Onset, Core, [InnerSyllable], Maybe Coda)

type Verb = (VerbType, Maybe Onset, [(Core, InnerCluster)], Maybe HighVowel)

type Ending = (Core, [InnerSyllable], Maybe Coda)

--TODO: This should mabye be a PointyList in the middle
type Intermediate = (VerbType, Maybe Onset, [(Core, InnerCluster)], Ending)

data Subject
    = Yo
    | Tú
    | Usted
    | Nosotros
    | Ustedes
    deriving (Show, Eq)

-- TODO: Gotta be a better name than that...
data SubjectlessTense
    = Infinitive
    | PastParticiple
    | PresentParticiple
    deriving (Show, Eq)

data SubjectSensativeTense
    = Conditional
    | Future
    | Imperfect
    | Present
    | Preterite
    | PresentSubjunctive
    | ImperfectSubjunctive
    deriving (Show, Eq)
