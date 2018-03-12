module Main where

import Control.Applicative
import qualified Control.Arrow as Arrow
import qualified Data.Maybe as Maybe

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

-- This should consider a special parser, rather than hard dropping
-- from a full word.
toEnding :: FullWord -> Ending
toEnding (_, core, innerSyllables, coda) = (core, innerSyllables, coda)

dropLastCore ::
       Core -> [InnerSyllable] -> (Maybe HighVowel, [(Core, InnerCluster)])
dropLastCore =
    let go built a [] = (fst a, built)
        go built a ((cluster, b):t) = go ((a, cluster) : built) b t
    in go []

toStem :: FullWord -> (Maybe HighVowel, Stem)
toStem (onset, core, innerSyllables, _) =
    let (mhv, l) = dropLastCore core innerSyllables
    in (mhv, (onset, l))

-- This is all pretty ugly and shows that these types are hardly
-- "transparent" to the user.  All these actions should be performed
-- through helper methods rather than destructuring and restructuring.
toIntermediate :: FullWord -> FullWord -> Intermediate
toIntermediate infinitive ending =
    let (mhv1, (mo, coreClusters_0)) = toStem infinitive
        ((mhv2, e), iss, coda) = toEnding ending
        (mhv, coreClusters1) =
            if Maybe.isJust mhv1 && Maybe.isJust mhv2
        -- This ugliness means that if the final syllable of the stem and
        -- the first syllable of the ending have a colliding dipthong (which
        -- must be a u/i) we add another syllable, where the u gets upgraded
        -- to the vowel, and the i becomes a y in the onset.
                then ( Nothing
                     , ( (Nothing, Left (False, U))
                       , Just (Nothing, Single (Regular Y))) :
                       coreClusters_0)
                else (mhv1 <|> mhv2, coreClusters_0)
    in ((mo, coreClusters1), ((mhv, e), iss, coda))

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

-- If we swap o/i in the tuple and constrain Monoid i => i
-- then this is just the composition (h (g (f a))) of three Applicatives
--   (->) a  - Reader or (a -> ...)
--   Maybe a
--   Monoid b => (b, a)
-- However, only Maybe appears to be an Alternative,
-- So that needs a custom definition
-- It would be nice to parameterize the error type, but 
-- then it's annoying to make add a separator
newtype Parser i o = Parser
    { runParser :: i -> Either String (o, i)
    }

instance Functor (Parser i) where
    fmap f p = Parser $ \input -> fmap (Arrow.first f) (runParser p input)

instance Applicative (Parser i) where
    pure x = Parser $ \a -> Right (x, a)
    pf <*> px =
        Parser $ \input ->
            case runParser pf input of
                Left e -> Left e
                Right (f, rest) ->
                    case runParser px rest of
                        Left e -> Left e
                        Right (x, o) -> Right (f x, o)

instance Monoid i => Alternative (Parser i) where
    empty = Parser (const (Left mempty))
    (<|>) p0 p1 =
        Parser $ \input ->
            case runParser p0 input of
                Left e0 ->
                    case runParser p1 input of
                        Left e1 -> Left (e0 ++ "\n" ++ e1)
                        secondResult -> secondResult
                firstResult -> firstResult

satisfy :: (String, Char -> Bool) -> Parser String Char
satisfy (msg, pred) =
    Parser $ \input ->
        case input of
            (c:cs)
                | pred c -> Right (c, cs)
            _ -> Left (msg ++ " at: '" ++ input ++ "'.")

lookAhead :: (Char -> Bool) -> Parser String ()
lookAhead pred =
    Parser $ \input ->
        case input of
            (c:cs)
                | pred c -> Right ((), input)
            _ -> Left ("lookAhead failed at: '" ++ input ++ "'.")

terminal :: Parser String ()
terminal =
    Parser $ \input ->
        if input == ""
            then Right ((), input)
            else Left
                     ("String did not terminate.  Remainder: '" ++
                      input ++ "'. ")

char :: Char -> Parser String Char
char c = satisfy ("Char did not match '" ++ [c] ++ "'", (==) c)

($>) :: Functor f => f a -> b -> f b
($>) = flip (fmap . const)

a :: Parser String LowVowel
a = char 'a' $> A

aa :: Parser String (Accentable LowVowel)
aa = char 'á' $> (True, A)

e :: Parser String LowVowel
e = char 'e' $> E

ae :: Parser String (Accentable LowVowel)
ae = char 'é' $> (True, E)

o :: Parser String LowVowel
o = char 'o' $> O

ao :: Parser String (Accentable LowVowel)
ao = char 'ó' $> (True, O)

i :: Parser String HighVowel
i = char 'i' $> I

ai :: Parser String (Accentable HighVowel)
ai = char 'í' $> (True, I)

u :: Parser String HighVowel
u
  -- An ü should never be the main vowel...
  -- But this is later used and can be upgraded
  -- In practice, this likely can't happen
 = (char 'u' <|> char 'ü') $> U

au :: Parser String (Accentable HighVowel)
au = char 'ú' $> (True, U)

lowVowel :: Parser String LowVowel
lowVowel = a <|> e <|> o

highVowel :: Parser String HighVowel
highVowel = i <|> u

accentableHighVowel :: Parser String (Accentable HighVowel)
accentableHighVowel = fmap ((,) False) highVowel <|> ai <|> au

accentableLowVowel :: Parser String (Accentable LowVowel)
accentableLowVowel = fmap ((,) False) lowVowel <|> aa <|> ae <|> ao

core :: Parser String Core
core =
    let ohv = optional highVowel
        either a b = fmap Left a <|> fmap Right b
        lowAndMaybeHigh = liftA2 (,) accentableLowVowel ohv
    -- We can almost get this with a single "rule"
    -- however, if an 'i/u' was scooped up by the ohv, and it was
    -- the main vowel, we'll fail the parse without the second
    -- rule that looks for that specific case.
    in liftA2 (,) ohv (either accentableHighVowel lowAndMaybeHigh) <|>
       fmap ((,) Nothing . Left) accentableHighVowel

notEOrI :: Char -> Bool
notEOrI c = c /= 'e' && c /= 'é' && c /= 'i' && c /= 'í'

eOrI :: Char -> Bool
eOrI = not . notEOrI

liquid :: Parser String Liquid
liquid = (char 'r' $> R) <|> (char 'l' $> L)

gFromG :: Parser String GCreator
gFromG = (char 'g' <* (lookAhead notEOrI <|> terminal)) $> GFromG

gFromGu :: Parser String GCreator
gFromGu = (char 'g' <* char 'u' <* lookAhead eOrI) $> GFromGU

g :: Parser String StopOrF
g
  -- Order is critical here
 = fmap G (gFromGu <|> gFromG)

kFromQu :: Parser String KCreator
kFromQu = (char 'q' <* char 'u' <* lookAhead eOrI) $> KFromQU

kFromC :: Parser String KCreator
kFromC
  -- Pretty much just loan words that allow terminal 'c'
  -- But included for consistency with allowking a terminal 'k'
 =
    (char 'c' <* ((lookAhead notEOrI *> lookAhead (/= 'h')) <|> terminal)) $>
    KFromC

kFromK :: Parser String KCreator
kFromK = char 'k' $> KFromK

k :: Parser String StopOrF
k = fmap K (kFromQu <|> kFromC <|> kFromK)

stopOrF :: Parser String StopOrF
stopOrF =
    char 'b' $> B <|> char 'd' $> D <|> char 'f' $> F <|> g <|> k <|>
    char 'p' $> P <|>
    char 't' $> T

sFromZ :: Parser String SCreator
sFromZ = char 'z' $> SFromZ

sFromC :: Parser String SCreator
sFromC = (char 'c' <* lookAhead eOrI) $> SFromC

sFromS :: Parser String SCreator
sFromS = char 's' $> SFromS

sFromX :: Parser String SCreator
sFromX = char 'x' $> SFromX

s :: Parser String Regular
s = fmap S (sFromZ <|> sFromC <|> sFromS <|> sFromX)

xFromJ :: Parser String XCreator
xFromJ = char 'j' $> XFromJ

xFromG :: Parser String XCreator
xFromG = (char 'g' <* lookAhead eOrI) $> XFromG

x :: Parser String Regular
x = fmap X (xFromJ <|> xFromG)

ch :: Parser String Regular
ch = char 'c' *> char 'h' $> CH

regular :: Parser String Regular
regular =
    ch <|> char 'm' $> M <|> char 'n' $> N <|> char 'ñ' $> Ñ <|> char 'y' $> Y <|>
    s <|>
    char 'v' $> V <|>
    x

consonant :: Parser String Consonant
consonant
  -- Order might matter here, but I don't think so
 = fmap Liquid liquid <|> fmap StopOrF stopOrF <|> fmap Regular regular

onset :: Parser String Onset
onset
  -- Order matters here
 = liftA2 Double stopOrF liquid <|> fmap Single consonant

coda :: Parser String Coda
coda = fmap (Coda True) (consonant <* char 's') <|> fmap (Coda False) consonant

syllable
  -- Order is critical here
  -- Not 100% sure that OnsetAndCore and CoreAndCoda are in
  -- the correct order, we would prefer to create an onset though...
 =
    liftA3 OnsetCoreAndCoda onset core coda <|> liftA2 OnsetAndCore onset core <|>
    liftA2 CoreAndCoda core coda <|>
    fmap CoreOnly core

innerSyllable :: Parser String InnerSyllable
innerSyllable
  -- precedence rules here get a little strange
  -- we need to make sure that we first match _just_ an onset
  -- and only try to match a coda + onset if we have to
 =
    let optionalPThenCore :: Parser String b -> Parser String (Maybe b, Core)
        optionalPThenCore = flip (liftA2 (,)) core . optional
        onsetNoCoda :: Parser String (Maybe a, Onset)
        onsetNoCoda = fmap ((,) Nothing) onset
        onsetAndCoda :: Parser String (Maybe Coda, Onset)
        onsetAndCoda = liftA2 (,) (fmap Just coda) onset
    in optionalPThenCore onsetNoCoda <|> optionalPThenCore onsetAndCoda

liftA4 ::
       Applicative a
    => (b -> c -> d -> e -> f)
    -> a b
    -> a c
    -> a d
    -> a e
    -> a f
liftA4 f a0 a1 a2 a3 = f <$> a0 <*> a1 <*> a2 <*> a3

word :: Parser String FullWord
word = liftA4 (,,,) (optional onset) core (many innerSyllable) (optional coda)

wordOnly :: Parser String FullWord
wordOnly = word <* terminal

main :: IO String
main = pure (show (runParser wordOnly "esternocleidooccipitomastoideos"))
