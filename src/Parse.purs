module Parse where

import Parsing
import Parsing.Combinators
import Parsing.String
import Parsing.String.Basic
import Prelude
import Util
import Word
import Note

import Data.Array.NonEmpty (NonEmptyArray, fromArray, cons', foldl1, zipWith)
import Data.Array.Partial (head, tail)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List.Types (toList)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat)
import Data.Rational (Rational)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toLower, toUpper)
import Data.Tuple (Tuple(..))

newtype TimeSig = TimeSig Rational

type Settings =
    { timeSig :: TimeSig
    , defDuration :: Duration
    , defNote :: Note
    }

type ParseFn a = Parser String a

parse :: String -> Either String (Tuple Settings (Array Word))
parse _ = Left "" -- TODO

{- Helpers -}

capString :: String -> ParseFn Boolean
capString s = foldl (\x y -> (&&) <$> x <*> y) (pure false) $ map eitherCase chars
    where chars = toCharArray s
          eitherCase c = (string <<< toLower $ c') $> false
                     <|> (string <<< toUpper $ c') $> true
              where c' = singleton c

{- Productions -}

-- sentence => (spaces word spaces)+

-- parseSentence :: ParseFn (Array Word)

-- word => abs-word | rel-word | complete-word

-- parseWord :: ParseFn Word

-- abs-word => [modifier] time-artic
--           | [modifier] time
--           | [modifier] time-spec spaces time

-- time-artic => spelled-number
--             | "e"
--             | "and"
--             | "a" | "ah"

-- parseTimeArtic :: ParseFn Word

-- parseAbsWord :: ParseFn Word

-- time => number | spelled-number
--       | "e"
--       | "and" | "&" | "+"
--       | "a" | "ah"

-- parseTime :: ParseFn Time

-- time-spec => "[" time "]"

-- parseTimeSpec :: ParseFn Time

-- spelled-number => "one" | "two" | "three" | "four" | "five" | "six"
--                 | "seven" | "eight" | "nine" | "ten" | "eleven" | "twelve"

parseSpelledNumber :: ParseFn (Tuple Natural Boolean) -- (number, isAccented)
parseSpelledNumber = foldl1 (<|>) alts
    where numWords = "one" `cons'` ["two", "three", "four", "five", "six", "seven",
                              "eight", "nine", "ten", "eleven", "twelve"]
          numVals = map intToNat (1 `cons'` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
          alts = zipWith toAlt numWords numVals :: NonEmptyArray (ParseFn (Tuple Natural Boolean))
          toAlt s n = Tuple n <$> capString s

-- number => [0-9]+

parseNumber :: ParseFn Natural
parseNumber = intToNat <.> stoi' =<< charListToStr <<< toList <$> many1 digit
    where stoi' :: String -> ParseFn Int
          stoi' s = case fromString s of
              Just 0 -> fail $ "0 is an invalid beat"
              Just i -> pure i
              Nothing -> fail $ "Number out of bounds: `" <> s <> "`"

-- rel-word => rudiment
--           | [modifier] misc-sound
--           | word-group

-- parseRelWord :: ParseFn Word

-- word-group => "(" (spaces rel-word spaces)+ ")"

-- parseWordGroup :: ParseFn Word

-- spaces => (' ' | '\t' | '\n' | ...)*

parseSpaces :: ParseFn Unit
parseSpaces = skipSpaces

-- complete-word => time-spec rel-word

-- parseCompleteWord :: ParseFn Word

-- rudiment => "tripulet" | "tpl"
--           | {"para"|"flama"|"draga"}{"diddle"} | {"pa"|"fa"|"dra"}{"dd"}
--           | "flamtap" | "ft"
--           | "flamaccent" | "fas"
--           | "flamacue" | "fac"
--           | "pataflafla" | "ptff"
--           | "twentyfive" | "ttf"
--           | "ratamacue" | "rtmc"
-- * any syllable of a rudiment may have a modifier

-- parseRudiment :: ParseFn Word

-- misc-sound => "ta" | "da"
--             | "tuh" | "duh"

-- parseMiscSound :: ParseFn Word

-- stroke => "tap" | "t"
--         | "gock" | "x"
--         | "buzz" | "z"
--         | "flam" | "f"
--         | "drag" | "dr"
--         | "double" | "d" | "="

-- parseStroke :: ParseFn Word

-- modifier => { mod-flag+ }

-- parseModifier :: ParseFn (Array Note)

-- mod-flag => "z" | "=" | ">" | "^" | "l" | "r" | "L" | "R"

-- parseModFlag :: ParseFn Note

-- * in any word-returning productions, dashes ('-')
--   between syllables are ignored, and capitilization of a
--   syllable yeilds accented articulation
