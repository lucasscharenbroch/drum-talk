module Parse where

import Control.Monad.Reader
import Note
import Parsing
import Parsing.Combinators
import Parsing.String
import Parsing.String.Basic
import Prelude hiding (between)
import Tree
import Util
import Word

import Control.Monad.Trans.Class (lift)
import Data.Array (elem, fromFoldable, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, cons', foldl1, zipWith) as NonEmpty
import Data.Array.Partial (head, tail)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Enum (defaultToEnum)
import Data.Foldable (foldl)
import Data.Int (fromString, round)
import Data.List.Types (toList)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.Rational (Rational, (%), numerator, denominator)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toLower, toUpper)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import JS.BigInt (fromInt, toNumber)

data TimeSig = TimeSig Natural Natural

sigToR :: TimeSig -> Rational
sigToR (TimeSig n d) = natToInt n % natToInt d

sigDenom :: TimeSig -> Int
sigDenom (TimeSig n d) = natToInt d

type Settings =
    { timeSig :: TimeSig
    , minDuration :: Duration
    , defDuration :: Duration
    , defNote :: Note
    }

type ParseFn a = ParserT String (Reader Settings) a

parse :: Settings -> String -> Either String (Array Word)
parse settings str = bimap show id $ runReader (runParserT str parseSentence) settings

{- Helpers -}

_capString :: String -> ParseFn Boolean
_capString s = foldl (\x y -> (||) <$> x <*> y) (pure false) $ map eitherCase chars
    where chars = toCharArray s
          eitherCase c = (string <<< toLower $ c') $> false
                     <|> (string <<< toUpper $ c') $> true
                     <?> ("word: `" <> s <> "`")
              where c' = singleton c


-- don't consume upon fail
capString :: String -> ParseFn Boolean
capString = try <<< _capString

-- don't consume upon fail, fail when followed by letter
capString' :: String -> ParseFn Boolean
capString' s = try $ _capString s <* notFollowedBy letter

natToTime :: Natural -> ParseFn Time
natToTime n = do
    {timeSig} <- ask
    pure $ MeasureOffset ((natToInt n - 1) % sigDenom timeSig)

noteToTree :: Note -> Tree WeightedNote
noteToTree = Leaf <<< ((flip WeightedNote) n1)

timePlusOffsetToWord :: Note -> Time -> Time -> ParseFn Word
timePlusOffsetToWord note (MeasureOffset m) (BeatOffset b) = do
    {timeSig} <- ask
    let denom = sigDenom timeSig
    pure $ AbsoluteWord (MeasureOffset $ m + (b * (1 % denom))) note
timePlusOffsetToWord _ _ _ = fail $ "Can't specify a time-spec on an absolute note unless the" <>
                                     "time-spec is measure-relative and the absolute note is beat-relative"

{- Productions -}

-- sentence => (spaces word spaces)+ eof

parseSentence :: ParseFn (Array Word)
parseSentence = fromFoldable <<< toList <$> many1 (parseSpaces *> parseWord <* parseSpaces) <* eof

-- word => [time-spec] duration-word

parseWord :: ParseFn Word
parseWord = (parseTimeSpec <* parseSpaces >>= _withTimeSpec)
        <|> _withoutTimeSpec
        <?> "word"
    where _withoutTimeSpec = parseDurationWord
          _withTimeSpec time = do
              word <- parseDurationWord
              case word of
                  (AbsoluteWord offset note) -> timePlusOffsetToWord note time offset
                  (RelativeWord tree duration) -> pure $ CompleteWord time tree duration
                  (CompleteWord _ _ _) -> fail "Can't specify a time twice"

-- duration-word => [duration-spec] modified-word
--                | [duration-spec] word-group

parseDurationWord :: ParseFn Word
parseDurationWord = (parseDurationSpec <* parseSpaces >>= _withDuration)
                <|> _withoutDuration
    where _withoutDuration = parseModifiedWord <|> parseWordGroup <|> parseRudiment
          _withDuration d = do
              word <- parseModifiedWord <|> parseWordGroup <|> parseRudiment
              case word of
                  (AbsoluteWord time note) -> pure $ CompleteWord time (noteToTree note) d
                  (RelativeWord tree _) -> pure $ RelativeWord tree d
                  (CompleteWord time tree _) -> pure $ CompleteWord time tree d
          parseWordGroup = pure unit >>= (\_ -> _parseWordGroup unit)

-- modified-word => [modifier] time-artic
--                | [modifier] time
--                | [modifier] misc-sound
--                | [modifier] stroke
--                | [modifier] rudiment

parseModifiedWord :: ParseFn Word
parseModifiedWord = do
    {defNote, defDuration} <- ask
    let modDurNoteToWord mod duration note = RelativeWord (noteToTree $ mod note) duration
    let _parseModified mod = (\(Tuple t n) -> AbsoluteWord t $ mod n) <$> parseTimeArtic
                        <|> (flip AbsoluteWord $ mod defNote) <$> parseTime
                        <|> uncurry (modDurNoteToWord mod) <$> parseMiscSound
                        <|> modDurNoteToWord mod defDuration <$> parseStroke
    option id parseModifier >>= _parseModified

-- time-artic => spelled-number
--             | "e"
--             | "and"
--             | "a" | "ah"

parseTimeArtic :: ParseFn (Tuple Time Note)
parseTimeArtic = ((\(Tuple n b) -> natToTime n >>= \n' -> defNoteWithAccent n' b) =<< parseSpelledNumber)
             <|> (defNoteWithAccent (BeatOffset (1 % 4)) =<< capString' "e")
             <|> (defNoteWithAccent (BeatOffset (2 % 4)) =<< capString' "and")
             <|> (defNoteWithAccent (BeatOffset (3 % 4)) =<< capString' "ah")
             <|> (defNoteWithAccent (BeatOffset (3 % 4)) =<< capString' "a")
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
parseTime = (natToTime =<< parseNumber)
        <|> ((natToTime <<< fst) =<< parseSpelledNumber)
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
parseSpelledNumber = NonEmpty.foldl1 (<|>) alts
    where numWords = "one" `NonEmpty.cons'` ["two", "three", "four", "five", "six", "seven",
                              "eight", "nine", "ten", "eleven", "twelve"]
          numVals = map (intToNat) (1 `NonEmpty.cons'` [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
          alts = NonEmpty.zipWith toAlt numWords numVals :: NonEmpty.NonEmptyArray (ParseFn (Tuple Natural Boolean))
          toAlt s n = Tuple n <$> capString' s

-- number => [0-9]+

parseNumber :: ParseFn Natural
parseNumber = intToNat <.> stoi' =<< charListToStr <<< toList <$> many1 digit
    where stoi' :: String -> ParseFn Int
          stoi' s = case fromString s of
              Just 0 -> fail $ "0 is an invalid beat/duration"
              Just i -> pure i
              Nothing -> fail $ "Number out of bounds: `" <> s <> "`"

-- word-group => "(" (spaces duration-word spaces)+ ")"

-- (monad deferred to avoid circular value dependency)
_parseWordGroup :: Unit -> ParseFn Word
_parseWordGroup _ = (fromFoldable <<< toList <$> inParens (many1 $  parseSpaces*> parseDurationWord <* parseSpaces)) >>= wrapUp
    where inParens = between (string "(") (string ")")
          wrapUp :: Array Word -> ParseFn Word
          wrapUp words = do
              {defDuration} <- ask
              let toTup (RelativeWord t d) = pure $ Tuple t d
                  toTup _ = fail "Can't use word with explicit time in group"
              tree <- tupsToTree <$> traverse toTup words
              pure $ RelativeWord tree defDuration
          ratsToNats :: Array Rational -> Array Natural
          ratsToNats rs = map (intToNat <<< round <<< toNumber <<< (\r -> numerator r * (_lcm / denominator r))) rs
              where _lcm = foldl lcm (fromInt 1) (map denominator rs)
          normalize :: Tuple (Tree WeightedNote) Duration -> Rational
          normalize (Tuple tree (Duration r)) = r * (1 % foldl (+) 0 (map (natToInt <<< getWeight) $ flattenTree tree))
          mulWeight x (WeightedNote note weight) = WeightedNote note (x * weight)
          mulWeight x (WeightedRest weight) = WeightedRest (x * weight)
          tupsToTree :: Array (Tuple (Tree WeightedNote) Duration) -> Tree WeightedNote
          tupsToTree tups = Internal $ zipWith (\t n -> (mulWeight n) <$> t) (map fst $ tups) (ratsToNats <<< map normalize $ tups)

-- spaces => (' ' | '\t' | '\n' | ...)*

parseSpaces :: ParseFn Unit
parseSpaces = skipSpaces

-- rudiment => "flamtap" | "ft"
--           | "flamaccent" | "fac"
--           | "flamacue" | "face"
--           | "pataflafla" | "ptff"
--           | "twentyfive" | "ttf"
--           | "ratamacue" | "rtmc"
--           | paradiddle
--           | triplet
-- * any syllable of a rudiment may have a modifier

type RudimentFragment =
    { long :: String
    , short :: String
    , duration :: Natural
    , trans :: Note -> Note
    }

rudiments :: Array (Array RudimentFragment)
rudiments = [
    [frag "flam" "f" n1 rf, frag "tap" "t" n1 r],
    [frag "flam" "f" n1 rf, frag "ac" "a" n1 l, frag "cent" "c" n1 r],
    [frag "flam" "f" n1 rf, frag "a" "a" n1 (acc <<< l), frag "cu" "c" n1 r, frag "e" "e" n1 l],
    [frag "pa" "p" n1 rf, frag "ta" "t" n1 l, frag "fla" "f" n1 r, frag "fla" "f" n1 lf],
    [frag "twen" "t" n1 rd, frag "ty" "t" n1 l, frag "five" "f" n2 r],
    [frag "ra" "r" n1 rd, frag "ta" "t" n1 l, frag "ma" "m" n1 r, frag "cue" "c" n3 l],
    [frag "par" "p" n1 r, frag "a" "a" n1 l, frag "par" "p" n1 r, frag "a" "a" n1 l, frag "par" "p" n1 r, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "par" "p" n1 r, frag "a" "a" n1 l, frag "par" "p" n1 r, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "par" "p" n1 r, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "par" "p" n1 r, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "flam" "f" n1 rf, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "drag" "dr" n1 rf, frag "a" "a" n1 l, frag "did" "d" n1 r, frag "dle" "d" n1 r],
    [frag "tri" "t" n1 r, frag "pu" "p" n1 l, frag "let" "l" n1 r]
]
    where frag long short duration trans = {long, short, duration, trans}
          flam n = n {numGraceNotes = n1}
          drag n = n {numGraceNotes = n2}
          r n = n {stick = WeakRight}
          l n = n {stick = WeakLeft}
          rf = flam <<< r
          lf = flam <<< l
          rd = drag <<< r
          acc n = n {articulation = Accent}

parseRudiment :: ParseFn Word
parseRudiment = do
    {defNote, defDuration} <- ask
    let defNote' isAccented = if isAccented
                              then defNote {articulation = Accent}
                              else defNote
    let mkRelWord tree = RelativeWord tree (defDuration * (Duration (2 % 1))) -- every rudiment takes up 2 * defDuration
    let _fToPf fToKey f = (\b -> WeightedNote (f.trans $ defNote' b) f.duration) <$> capString (fToKey f) <* many (char '-')
    let fToPfShort = _fToPf (\f -> f.short)
    let fToPfLong = _fToPf (\f -> f.long)
    let rToPf fToPf fs = mkRelWord <<< Internal <$> foldl (\acc frag -> (<>) <$> acc <*> (pure <<< Leaf <$> fToPf frag)) (pure []) fs <* notFollowedBy letter
    foldl (\x y -> x <|> try (rToPf fToPfShort y) <|> try (rToPf fToPfLong y)) (fail "") rudiments
    <?> "rudiment"

-- misc-sound => "ta" | "da"
--             | "tuh" | "duh"

parseMiscSound :: ParseFn (Tuple Duration Note)
parseMiscSound = do
    {defDuration, defNote} <- ask
    let halfDefDuration = defDuration / (Duration $ 2 % 1)
    let defNote' isAccented = if isAccented
                              then defNote {articulation = Accent}
                              else defNote
    (    Tuple defDuration <<< defNote' <$> capString' "ta"
     <|> Tuple defDuration <<< defNote' <$> capString' "da"
     <|> Tuple halfDefDuration <<< defNote' <$> capString' "tuh"
     <|> Tuple halfDefDuration <<< defNote' <$> capString' "duh"
    )

-- stroke => "tap" | "t"
--         | "gock" | "x"
--         | "buzz" | "z"
--         | "flam" | "f"
--         | "drag" | "dr"
--         | "double" | "d" | "="

parseStroke :: ParseFn Note
parseStroke = do
    defNote <- (\settings -> settings.defNote) <$> ask
    let defNote' isAccented = if isAccented
                              then defNote {articulation = Accent}
                              else defNote
    (    (\b -> defNote' b)                        <$> capString' "tap"
     <|> (\b -> defNote' b)                        <$> capString' "t"
     <|> (\b -> (defNote' b) {stroke = Gock})      <$> capString' "gock"
     <|> (\b -> (defNote' b) {stroke = Gock})      <$> capString' "x"
     <|> (\b -> (defNote' b) {stroke = Buzz})      <$> capString' "buzz"
     <|> (\b -> (defNote' b) {stroke = Buzz})      <$> capString' "z"
     <|> (\b -> (defNote' b) {numGraceNotes = n1}) <$> capString' "flam"
     <|> (\b -> (defNote' b) {numGraceNotes = n1}) <$> capString' "f"
     <|> (\b -> (defNote' b) {numGraceNotes = n2}) <$> capString' "drag"
     <|> (\b -> (defNote' b) {numGraceNotes = n2}) <$> capString' "dr"
     <|> (\b -> (defNote' b) {stroke = Double})    <$> capString' "d"
     <|> (\b -> (defNote' b) {stroke = Double})    <$> capString' "="
    )

-- modifier => { mod-flag+ }

parseModifier :: ParseFn (Note -> Note)
parseModifier = foldl (flip (<<<)) id <$> inBraces (many parseModFlag)
    where inBraces = between (string "{") (string "}")

-- mod-flag => "z" | "=" | ">" | "^" | "l" | "r" | "L" | "R"

parseModFlag :: ParseFn (Note -> Note)
parseModFlag = string "z"  $> (\n -> n {stroke = Buzz})
           <|> string "="  $> (\n -> n {stroke = Double})
           <|> string "x"  $> (\n -> n {stroke = Gock})
           <|> string ">"  $> (\n -> n {articulation = Accent})
           <|> string "^"  $> (\n -> n {articulation = Marcato})
           <|> string "'"  $> (\n -> n {numGraceNotes = n1})
           <|> string "f"  $> (\n -> n {numGraceNotes = n1})
           <|> string "\"" $> (\n -> n {numGraceNotes = n2})
           <|> string "l"  $> (\n -> n {stick = WeakLeft})
           <|> string "r"  $> (\n -> n {stick = WeakRight})
           <|> string "L"  $> (\n -> n {stick = StrongLeft})
           <|> string "R"  $> (\n -> n {stick = StrongRight})
           <?> "modifier flag"

parseDurationSpec :: ParseFn Duration
parseDurationSpec = inAngles $ ((\n -> Duration (1 % natToInt n)) <$> parseNumber) >>= validateDuration
    where inAngles = between (string "<") (string ">")
          validateDuration d@(Duration r) = if d `elem` [d4, d8, d16, d32]
                                            then pure d
                                            else fail ("Invalid duration spec: " <> show r)
