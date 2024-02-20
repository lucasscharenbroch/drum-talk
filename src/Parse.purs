module Parse where

import Data.List
import Data.Tuple
import Data.Either
import Data.Rational
import Prelude
import Words
import Parsing

newtype TimeSig = TimeSig Rational

type Settings =
    { timeSig :: TimeSig
    , defDuration :: Duration
    , defNote :: Note
    }

type ParseFn a = Parser String a

parse :: String -> Either String (Tuple Settings (Array Word))
parse _ = Left "" -- TODO

{-

sentence => word+

word => abs-word | rel-word | complete-word

abs-word => [modifier] time
          | [modifier] time-spec time

time => number | spelled-number
      | "e"
      | "and" | "&" | "+"
      | "a" | "ah"

time-spec => "[" time "]"

spelled-number => "one" | "two" | "three" | "four" | "five" | "six"
                | "seven" | "eight" | "nine" | "ten" | "eleven" | "twelve"

number => [0-9]+

rel-word => rudiment
          | [modifier] misc-sound
          | word-group

word-group => "(" (space* rel-word space*)+ ")"

space => ' ' | '\t' | '\n'

complete-word => time-spec rel-word

rudiment => "tripulet" | "tpl"
          | {"para"|"flama"|"draga"}{"diddle"} | {"pa"|"fa"|"dra"}{"dd"}
          | "flamtap" | "ft"
          | "flamaccent" | "fas"
          | "flamacue" | "fac"
          | "pataflafla" | "ptff"
          | "twentyfive" | "ttf"
          | "ratamacue" | "rtmc"

* any syllable of a rudiment may have a modifier

misc-sound => "ta" | "da"
            | "tuh" | "duh"

stroke => "tap" | "t"
        | "gock" | "x"
        | "buzz" | "z"
        | "flam" | "f"
        | "drag" | "dr"
        | "double" | "d" | "="

modifier => { mod-flag+ }
mod-flag => "z" | "=" | ">" | "^" | "l" | "r"

* in any of the above word-returning productions, dashes ('-')
  between syllables are ignored, and capitilization of a
  syllable yeilds accented articulation

-}

-- sentence => word+

-- parseSentence :: ParseFn (Array Word)

-- word => abs-word | rel-word | complete-word

-- parseWord :: ParseFn Word

-- abs-word => [modifier] time
--           | [modifier] time-spec time

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

-- parseSpelledNumber :: ParseFn Natural

-- number => [0-9]+

-- parseNumber :: ParseFn Natural

-- rel-word => rudiment
--           | [modifier] misc-sound
--           | word-group

-- parseRelWord :: ParseFn Word

-- word-group => "(" (space* rel-word space*)+ ")"

-- parseWordGroup :: ParseFn Word

-- space => ' ' | '\t' | '\n'

-- parseSpace :: ParseFn Unit

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

-- mod-flag => "z" | "=" | ">" | "^" | "l" | "r"

-- parseModFlag :: ParseFn Note
