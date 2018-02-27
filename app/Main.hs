module Main where

import Control.Applicative

data High_Vowel
  = U
  | I
  deriving (Show)

data Low_Vowel
  = A
  | E
  | O
  deriving (Show)

type Accentable a = (Bool, a)

accent :: Accentable a -> Accentable a
accent (_, a) = (True, a)

unaccent :: Accentable a -> Accentable a
unaccent (_, a) = (False, a)

data High_Or_Low
  = High High_Vowel
  | Low Low_Vowel
  deriving (Show)

data Core
  = Vowel_Only (Accentable High_Or_Low)
  | Rising_Diphthong High_Vowel (Accentable High_Or_Low)
  | Falling_Diphthong (Accentable Low_Vowel) High_Vowel 
  | Tripththong High_Vowel (Accentable Low_Vowel) High_Vowel 
  deriving (Show)

data Liquid
  = R
  | L
  deriving (Show)

data Stop_Or_F
  = B
  | D
  | F
  | G G_Creator
  | K K_Creator
  | P
  | T
  deriving (Show)

data G_Creator
  = G_From_G
  | G_From_GU
  deriving (Show)

data K_Creator
  = K_From_QU
  | K_From_C
  | K_From_K
  deriving (Show)

data Regular
  = CH
  | M
  | N
  | Ñ
  | Y
  | S S_Creator
  | V
  | X X_Creator
  deriving (Show)

data S_Creator
  = S_From_Z
  | S_From_C
  | S_From_S
  | S_From_X
  deriving (Show)

data X_Creator
  = X_From_J
  | X_From_G
  deriving (Show)

data Consonant
  = Liquid Liquid
  | Stop_Or_F Stop_Or_F
  | Regular Regular
  deriving (Show)

data Onset
  = Single Consonant
  | Double Stop_Or_F Liquid
  deriving (Show)

data Coda
  = Coda Bool Consonant
  deriving (Show)

data Syllable
  = Core_Only Core
  | Onset_And_Core Onset Core
  | Core_And_Coda Core Coda
  | Onset_Core_And_Coda Onset Core Coda
  deriving (Show)

data Inner_Syllable
  = Inner_Core_Only Core
  | Inner_Onset_And_Core Onset Core
  | Inner_Coda_Onset_And_Core Coda Onset Core
  deriving (Show)

data Full_Word
  = Full_Word Syllable [Inner_Syllable]
  deriving (Show)

-- If we swap o/i in the tuple and constrain Monoid i => i
-- then this is just the composition (h (g (f a))) of three Applicatives
--   (->) a  - Reader or (a -> ...)
--   Maybe a
--   Monoid b => (b, a)
-- However, only Maybe appears to be an Alternative,
-- So that needs a custom definition
-- It would be nice to parameterize the error type, but 
-- then it's annoying to make add a separator
newtype Parser i o = Parser { run_parser :: i -> Either String (o, i) }

instance Functor (Parser i) where
  fmap f p = Parser $ \input ->
    fmap (\(o,i) -> (f o, i)) (run_parser p input)

instance Applicative (Parser i) where
  pure x = Parser $ \a -> Right (x, a)
  pf <*> px = Parser $ \input ->
    case run_parser pf input of
      Left e -> Left e
      Right (f, rest) -> case run_parser px rest of
        Left e -> Left e
        Right (x, o) -> Right (f x, o)

instance Monoid i => Alternative (Parser i) where
  empty = Parser (const (Left mempty))
  (<|>) p0 p1 = Parser $ \input ->
    case run_parser p0 input of
      Left e0 -> case run_parser p1 input of
        Left e1 -> Left (e0 ++ "\n" ++ e1)
        second_result -> second_result
      first_result -> first_result

satisfy :: (String, (Char -> Bool)) -> Parser String Char
satisfy (msg, pred) = Parser $ \input ->
  case input of
    (c:cs) | pred c -> Right (c, cs)
    _ -> Left (msg ++ " at: '" ++ input ++ "'.")

look_ahead :: (Char -> Bool) -> Parser String ()
look_ahead pred = Parser $ \input ->
  case input of
    (c:cs) | pred c -> Right ((), input)
    _ -> Left ("look_ahead failed at: '" ++ input ++ "'.")

terminal :: Parser String ()
terminal = Parser $ \input ->
  if input == "" then
    Right ((), input)
  else
    Left ("String did not terminate.  Remainder: '" ++ input ++ "'. ")

char :: Char -> Parser String Char
char c = satisfy ("Char did not match '" ++ [c] ++ "'", (==) c)

($>) :: Functor f => f a -> b -> f b
($>) = flip (fmap . const)

a :: Parser String Low_Vowel
a = char 'a' $> A

aa :: Parser String (Accentable Low_Vowel)
aa = char 'á' $> (True, A)

e :: Parser String Low_Vowel
e = char 'e' $> E

ae :: Parser String (Accentable Low_Vowel)
ae = char 'é' $> (True, E)

o :: Parser String Low_Vowel
o = char 'o' $> O

ao :: Parser String (Accentable Low_Vowel)
ao = char 'ó' $> (True, O)

i :: Parser String High_Vowel
i = char 'i' $> I

ai :: Parser String (Accentable High_Vowel)
ai = char 'í' $> (True, I)

u :: Parser String High_Vowel
u =
  -- An ü should never be the main vowel...
  -- But this is later used and can be upgraded
  -- In practice, this likely can't happen
  (char 'u' <|> char 'ü')  $> U

au :: Parser String (Accentable High_Vowel)
au = char 'ú' $> (True, U)

low_vowel :: Parser String Low_Vowel
low_vowel = a <|> e <|> o

high_vowel :: Parser String High_Vowel
high_vowel = i <|> u

high_or_low :: Parser String High_Or_Low
high_or_low = fmap Low low_vowel <|> fmap High high_vowel

accentable_high_or_low :: Parser String (Accentable High_Or_Low)
accentable_high_or_low =
  fmap (\(a,b) -> (a, Low b)) (aa <|> ae <|> ao)
    <|> fmap (\(a,b) -> (a, High b)) (ai <|> au)

accentable_low_vowel :: Parser String (Accentable Low_Vowel)
accentable_low_vowel =
  fmap ((,) False) low_vowel <|> aa <|> ae <|> ao

vowel :: Parser String (Accentable High_Or_Low)
vowel = fmap ((,) False) high_or_low <|> accentable_high_or_low

core :: Parser String Core
core =
  -- Order here is critical
  -- We can't jump to a more 'lax' alternative until we're sure
  -- that it wouldn't meet the more strict case.
  liftA3 Tripththong high_vowel accentable_low_vowel high_vowel
    <|> liftA2 Rising_Diphthong high_vowel vowel
    <|> liftA2 Falling_Diphthong accentable_low_vowel high_vowel
    <|> fmap Vowel_Only vowel

not_e_or_i :: Char -> Bool
not_e_or_i c = c /= 'e' && c /= 'é' && c /= 'i' && c /= 'í'

e_or_i :: Char -> Bool
e_or_i = not . not_e_or_i

liquid :: Parser String Liquid
liquid = (char 'r' $> R) <|> (char 'l' $> L)

g_from_g :: Parser String G_Creator
g_from_g =
  (char 'g' <* (look_ahead not_e_or_i <|> terminal)) $> G_From_G

g_from_gu :: Parser String G_Creator
g_from_gu = (char 'g' <* char 'u' <* look_ahead e_or_i) $> G_From_GU

g :: Parser String Stop_Or_F
g =
  -- Order is critical here
  fmap (G) (g_from_gu <|> g_from_g)

k_from_qu :: Parser String K_Creator
k_from_qu = (char 'q' <* char 'u' <* look_ahead e_or_i) $> K_From_QU

k_from_c :: Parser String K_Creator
k_from_c =
  -- Pretty much just loan words that allow terminal 'c'
  -- But included for consistency with allowking a terminal 'k'
  (char 'c' <* ((look_ahead not_e_or_i *> look_ahead (/= 'h'))<|> terminal)) $> K_From_C

k_from_k :: Parser String K_Creator
k_from_k = char 'k' $> K_From_K

k :: Parser String Stop_Or_F
k = fmap (K) (k_from_qu <|> k_from_c <|> k_from_k)

stop_or_f :: Parser String Stop_Or_F
stop_or_f =
  char 'b' $> B
    <|> char 'd' $> D
    <|> char 'f' $> F
    <|> g
    <|> k
    <|> char 'p' $> P
    <|> char 't' $> T

s_from_z :: Parser String S_Creator
s_from_z = char 'z' $> S_From_Z

s_from_c :: Parser String S_Creator
s_from_c = (char 'c' <* look_ahead e_or_i) $> S_From_C

s_from_s :: Parser String S_Creator
s_from_s = char 's' $> S_From_S

s_from_x :: Parser String S_Creator
s_from_x = char 'x' $> S_From_X

s :: Parser String Regular
s = fmap (S) (s_from_z <|> s_from_c <|> s_from_s <|> s_from_x)

x_from_j :: Parser String X_Creator
x_from_j = char 'j' $> X_From_J

x_from_g :: Parser String X_Creator
x_from_g = (char 'g' <* look_ahead e_or_i) $> X_From_G

x :: Parser String Regular
x = fmap (X) (x_from_j <|> x_from_g)

ch :: Parser String Regular
ch = char 'c' *> char 'h' $> CH

regular :: Parser String Regular
regular =
  ch
    <|> char 'm' $> M
    <|> char 'n' $> N
    <|> char 'ñ' $> Ñ
    <|> char 'y' $> Y
    <|> s
    <|> char 'v' $> V
    <|> x

consonant :: Parser String Consonant
consonant =
  -- Order might matter here, but I don't think so
  fmap Liquid liquid
    <|> fmap Stop_Or_F stop_or_f
    <|> fmap Regular regular

onset :: Parser String Onset
onset =
  -- Order matters here
  liftA2 Double stop_or_f liquid
    <|> fmap Single consonant

coda :: Parser String Coda
coda =
  fmap (Coda True) (consonant <* char 's')
   <|> fmap (Coda False) (consonant)

syllable =
  -- Order is critical here
  -- Not 100% sure that Onset_And_Core and Core_And_Coda are in
  -- the correct order, we would prefer to create an onset though...
  liftA3 Onset_Core_And_Coda onset core coda
    <|> liftA2 Onset_And_Core onset core
    <|> liftA2 Core_And_Coda core coda
    <|> fmap Core_Only core

inner_syllable :: Parser String Inner_Syllable
inner_syllable =
  -- Order here is important, but backwards from usual.
  -- We want to make sure we prefer an onset to a coda.
  fmap Inner_Core_Only core
   <|> liftA2 Inner_Onset_And_Core onset core
   <|> liftA3 Inner_Coda_Onset_And_Core coda onset core

liftA4 :: Applicative a => (b -> c -> d -> e -> f) -> a b -> a c -> a d -> a e -> a f
liftA4 f a0 a1 a2 a3 = f <$> a0 <*> a1 <*> a2 <*> a3

word :: Parser String Full_Word
word =
  -- This is really ugly...
   liftA4 (\a b c d -> Full_Word (Onset_Core_And_Coda a b d) c) onset core (many inner_syllable) coda
    <|> liftA3 (\a b c -> Full_Word (Onset_And_Core a b) c) onset core (many inner_syllable)
    <|> liftA3 (\a b c -> Full_Word (Core_And_Coda a c) b) core (many inner_syllable) coda
    <|> liftA2 (\a b -> Full_Word (Core_Only a) b) core (many inner_syllable)

word_only :: Parser String Full_Word
word_only = word <* terminal

main :: IO String
main = pure (show (run_parser word_only "esternocleidooccipitomastoideos"))
