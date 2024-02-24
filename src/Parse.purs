module Parse where

import Note
import Parsing
import Parsing.Combinators
import Parsing.String
import Parsing.String.Basic
import Prelude hiding (between)
import Util
import Word

import Data.Array.NonEmpty (NonEmptyArray, fromArray, cons', foldl1, zipWith)
import Data.Array.Partial (head, tail)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List.Types (toList)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.Rational (Rational, (%))
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toLower, toUpper)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Control.Monad.Reader

newtype TimeSig = TimeSig Rational

type Settings =
    { timeSig :: TimeSig
    , minDuration :: Duration
    , defDuration :: Duration
    , defNote :: Note
    }

type ParseFn a = ParserT String (Reader Settings) a

parse :: Settings -> String -> Either String (Array Word)
parse _ _ = Left "" -- TODO

{- Helpers -}

capString :: String -> ParseFn Boolean
capString s = foldl (\x y -> (&&) <$> x <*> y) (pure false) $ map eitherCase chars
    where chars = toCharArray s
          eitherCase c = (string <<< toLower $ c') $> false
                     <|> (string <<< toUpper $ c') $> true
              where c' = singleton c

capString' :: String -> ParseFn Boolean
capString' = try <<< capString

natToTime :: Natural -> Time
natToTime = (\n -> MeasureOffset ((natToInt n) % 1))

{- Productions -}

-- sentence => (spaces word spaces)+

-- parseSentence :: ParseFn (Array Word)

-- word => abs-word | rel-word | complete-word

-- parseWord :: ParseFn Word

-- abs-word => [modifier] time-artic
--           | [modifier] time
--           | [modifier] time-spec spaces time

parseAbsWord :: ParseFn Word
parseAbsWord = applyModifier <$> (option id $ try parseModifier) <*> _parseAbsWord
    where applyModifier modFn (Tuple time note) = AbsoluteWord time (modFn note)
          _parseAbsWord :: ParseFn (Tuple Time Note)
          _parseAbsWord = do
                  defNote <- (\settings -> settings.defNote) <$> ask
                  (parseTimeArtic
                   <|> (\t -> Tuple t defNote) <$> parseTime
                   <|> (join $ joinTimesIntoWord defNote <$> parseTimeSpec <*> (parseSpaces *> parseTime)))
          joinTimesIntoWord :: Note -> Time -> Time -> ParseFn (Tuple Time Note)
          joinTimesIntoWord note (MeasureOffset m) (BeatOffset b) = pure $ Tuple (MeasureOffset $ m + b) note
          joinTimesIntoWord _ _ _ = fail $ "Can't specify a time-spec on an absolute note unless the" <>
                                           "time-spec is measure-relative and the absolute note is beat-relative"

-- time-artic => spelled-number
--             | "e"
--             | "and"
--             | "a" | "ah"

parseTimeArtic :: ParseFn (Tuple Time Note)
parseTimeArtic = ((\(Tuple n b) -> defNoteWithAccent (natToTime n) b) =<< parseSpelledNumber)
             <|> (defNoteWithAccent (BeatOffset (1 % 4)) =<< capString' "e")
             <|> (defNoteWithAccent (BeatOffset (2 % 4)) =<< capString' "and")
             <|> (defNoteWithAccent (BeatOffset (3 % 4)) =<< capString' "a")
             <|> (defNoteWithAccent (BeatOffset (3 % 4)) =<< capString' "ah")
    where defNoteWithAccent :: Time -> Boolean -> ParseFn (Tuple Time Note)
          defNoteWithAccent time isAccented = do
              defNote <- (\settings -> settings.defNote) <$> ask
              let note = if isAccented
                         then defNote {articulation = Accent}
                         else defNote
              pure $ Tuple time note

-- time => number | spelled-number
--       | "e"
--       | "and" | "&" | "+"
--       | "a" | "ah"

parseTime :: ParseFn Time
parseTime = natToTime <$> parseNumber
        <|> natToTime <<< fst <$> parseSpelledNumber
        <|> BeatOffset (1 % 4) <$ capString' "e"
        <|> BeatOffset (1 % 4) <$ capString' "and"
        <|> BeatOffset (2 % 4) <$ string "&"
        <|> BeatOffset (2 % 4) <$ string "+"
        <|> BeatOffset (3 % 4) <$ capString' "ah"
        <|> BeatOffset (4 % 4) <$ capString' "a"

-- time-spec => "[" time "]"

parseTimeSpec :: ParseFn Time
parseTimeSpec = between (string "[") (string "]") parseTime

-- spelled-number => "one" | "two" | "three" | "four" | "five" | "six"
--                 | "seven" | "eight" | "nine" | "ten" | "eleven" | "twelve"

parseSpelledNumber :: ParseFn (Tuple Natural Boolean) -- (number, isAccented)
parseSpelledNumber = foldl1 (<|>) alts
    where numWords = "one" `cons'` ["two", "three", "four", "five", "six", "seven",
                              "eight", "nine", "ten", "eleven", "twelve"]
          numVals = map intToNat (1 `cons'` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
          alts = zipWith toAlt numWords numVals :: NonEmptyArray (ParseFn (Tuple Natural Boolean))
          toAlt s n = Tuple n <$> capString' s

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

parseModifier :: ParseFn (Note -> Note)
parseModifier = foldl (flip (<<<)) id <$> inBraces (many parseModFlag)
    where inBraces = between (string "{") (string "}")

-- mod-flag => "z" | "=" | ">" | "^" | "l" | "r" | "L" | "R"

parseModFlag :: ParseFn (Note -> Note)
parseModFlag = string "z" $> (\n -> n {stroke = Buzz})
           <|> string "=" $> (\n -> n {stroke = Double})
           <|> string ">" $> (\n -> n {articulation = Accent})
           <|> string "^" $> (\n -> n {articulation = Marcato})
           <|> string "l" $> (\n -> n {stick = WeakLeft})
           <|> string "r" $> (\n -> n {stick = WeakRight})
           <|> string "L" $> (\n -> n {stick = StrongLeft})
           <|> string "R" $> (\n -> n {stick = StrongRight})

-- * in any word-returning productions, dashes ('-')
--   between syllables are ignored, and capitilization of a
--   syllable yeilds accented articulation
