module Linguistics.Parsers
    ( a
    , aa
    , e
    , ae
    , o
    , ao
    , i
    , ai
    , u
    , au
    , lowVowel
    , highVowel
    , accentableHighVowel
    , accentableLowVowel
    , core
    , notEOrI
    , eOrI
    , liquid
    , gFromG
    , gFromGu
    , g
    , hardCFromQu
    , hardCFromC
    , hardC
    , stopOrF
    , softCFromZ
    , softCFromC
    , softC
    , softG
    , ch
    , regular
    , consonant
    , onset
    , coda
    , innerSyllable
    , onlyInnerSyllable'
    , word
    , ending
    , wordOnly
    , endingOnly
    ) where

import Control.Applicative ((<|>), liftA2, liftA3, many, optional)
import Linguistics.Types
import Parser
import Parser.Utils (char, lookAhead, terminal)
import Utils (($>), liftA4, swapTuple)

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
    let optionalHighVowel = optional highVowel
        eitherP l r = fmap Left l <|> fmap Right r
        lowAndMaybeHigh = liftA2 (,) accentableLowVowel optionalHighVowel
        -- the accent info will be _inside_ the either, and we need a utility
        -- to pull it out.
        extractAccent eith =
            case eith of
                Left (b, l) -> (b, Left l)
                Right ((b, r0), r1) -> (b, Right (r0, r1))
        vowelAndRightDiph =
            fmap extractAccent (eitherP accentableHighVowel lowAndMaybeHigh)
    -- We can almost get this with a single "rule"
    -- however, if an 'i/u' was scooped up by the optionalHighVowel, and it was
    -- the main vowel, we'll fail the parse without the second
    -- rule that looks for that specific case.
    in liftA2 (curry swapTuple) optionalHighVowel vowelAndRightDiph <|>
       fmap (fmap ((,) Nothing . Left)) accentableHighVowel

notEOrI :: Char -> Bool
notEOrI c = c /= 'e' && c /= 'é' && c /= 'i' && c /= 'í'

eOrI :: Char -> Bool
eOrI = not . notEOrI

liquid :: Parser String Liquid
liquid = (char 'r' $> R) <|> (char 'l' $> L)

gFromG :: Parser String StopOrF
gFromG = (char 'g' <* (lookAhead notEOrI <|> terminal)) $> HardG

gFromGu :: Parser String StopOrF
gFromGu = (char 'g' <* char 'u' <* lookAhead eOrI) $> HardG

g :: Parser String StopOrF
g
  -- Order is critical here
 = gFromGu <|> gFromG

hardCFromQu :: Parser String StopOrF
hardCFromQu = (char 'q' <* char 'u' <* lookAhead eOrI) $> HardC

hardCFromC :: Parser String StopOrF
hardCFromC
  -- Pretty much just loan words that allow terminal 'c'
  -- But included for consistency with allowking a terminal 'k'
 =
    (char 'c' <* ((lookAhead notEOrI *> lookAhead (/= 'h')) <|> terminal)) $>
    HardC

hardC :: Parser String StopOrF
hardC = hardCFromQu <|> hardCFromC

stopOrF :: Parser String StopOrF
stopOrF =
    char 'b' $> B <|> char 'd' $> D <|> char 'f' $> F <|> g <|> char 'k' $> K <|>
    hardC <|>
    char 'p' $> P <|>
    char 't' $> T

softCFromZ :: Parser String Regular
softCFromZ = char 'z' $> SoftC

softCFromC :: Parser String Regular
softCFromC = (char 'c' <* lookAhead eOrI) $> SoftC

softC :: Parser String Regular
softC = softCFromZ <|> softCFromC

softG :: Parser String Regular
softG = (char 'g' <* lookAhead eOrI) $> SoftG

ch :: Parser String Regular
ch = char 'c' *> char 'h' $> CH

regular :: Parser String Regular
regular =
    ch <|> softC <|> softG <|> char 'h' $> H <|> char 'j' $> J <|> char 'm' $> M <|>
    char 'n' $> N <|>
    char 'ñ' $> Ñ <|>
    char 's' $> S <|>
    char 'y' $> Y <|>
    char 'x' $> X <|>
    char 'v' $> V

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

onsetNoCoda :: Parser String (Maybe a, Onset)
onsetNoCoda = fmap ((,) Nothing) onset

onsetAndCoda :: Parser String (Maybe Coda, Onset)
onsetAndCoda = liftA2 (,) (fmap Just coda) onset

consonantAndS :: Parser String (Maybe Coda, Onset)
consonantAndS =
    liftA2
        (const . flip (,) (Single (Regular S)) . Just . Coda False)
        consonant
        (char 's')

innerSyllable :: Parser String InnerSyllable
innerSyllable
  -- precedence rules here get a little strange
  -- we need to make sure that we first match _just_ an onset
  -- and only try to match a coda + onset if we have to
  -- And even then, in the case of /.s/ (like in "pensar")
  -- it will match a coda completely, and then fail, so we need
  -- another rule (consonantAndS) to protect that one.
 =
    let optionalPThenCore :: Parser String b -> Parser String (Maybe b, Core)
        optionalPThenCore = flip (liftA2 (,)) core . optional
    in optionalPThenCore onsetNoCoda <|> optionalPThenCore onsetAndCoda <|>
       optionalPThenCore consonantAndS

onlyInnerSyllable' :: Parser String InnerSyllable'
onlyInnerSyllable' =
    liftA2
        (,)
        core
        (optional
             (onsetNoCoda <* terminal <|> onsetAndCoda <* terminal <|>
              consonantAndS <* terminal))

word :: Parser String FullWord
word = liftA4 (,,,) (optional onset) core (many innerSyllable) (optional coda)

ending :: Parser String Ending
ending = liftA3 (,,) core (many innerSyllable) (optional coda)

wordOnly :: Parser String FullWord
wordOnly = word <* terminal

endingOnly :: Parser String Ending
endingOnly = ending <* terminal
