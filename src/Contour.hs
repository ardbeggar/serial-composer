{-|
Module      : Contour
Description : Robert Morris's Melodic Contour Segments (CSeg), COM-Matrix, and Prime Contour equivalence
Stability   : experimental

Implements melodic contour analysis as formalized by Robert Morris and
Elizabeth West Marvin. Converts ordered pitch sequences (MIDI numbers,
note+octave names like B2,F3,C3,Eb3, or raw contour ranks) into relative
height segments (CSeg), computes the 2D Comparison Matrix (COM), derives
P/R/I/RI contour transformations, and determines Prime Contour equivalence.
-}
module Contour
  ( ContourSegment
  , ContourAnalysis(..)
  , parseContourInput
  , toCSeg
  , contourRetrograde
  , contourInversion
  , contourRI
  , primeContour
  , comMatrix
  , analyzeContour
  , areContoursEquivalent
  ) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace, toUpper)
import Data.List (elemIndex, nub, sort)
import Text.Read (readMaybe)

type ContourSegment = [Int]

data ContourAnalysis = ContourAnalysis
  { caInputLabels :: [String]       -- ^ Original input tokens
  , caRawValues   :: [Int]          -- ^ Parsed MIDI / height values
  , caCSeg        :: ContourSegment -- ^ Normalized contour segment <0 .. maxRank>
  , caRetrograde  :: ContourSegment -- ^ R(CSeg)
  , caInversion   :: ContourSegment -- ^ I(CSeg)
  , caRI          :: ContourSegment -- ^ RI(CSeg)
  , caPrime       :: ContourSegment -- ^ Lexicographically smallest of {P, R, I, RI}
  , caSymmetry    :: [String]       -- ^ Which transformations equal P (e.g., ["P", "RI"])
  , caCOM         :: [[Char]]       -- ^ 2D Comparison Matrix ('+', '-', '0')
  } deriving (Show, Eq)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitCommas :: String -> [String]
splitCommas s =
  case break (== ',') s of
    (chunk, "")       -> [chunk]
    (chunk, _ : rest) -> chunk : splitCommas rest

parsePitchClassName :: String -> Maybe Int
parsePitchClassName str =
  let norm = map toUpper str
      flats = ["C", "DB", "D", "EB", "E", "F", "GB", "G", "AB", "A", "BB", "B"]
      sharps = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in elemIndex norm flats <|> elemIndex norm sharps

-- | Parses either a plain integer (MIDI pitch or contour rank, e.g. "47" or "3")
-- or an octave-qualified note name (e.g. "B2", "Eb3", "F#4", "C-1").
parseHeightToken :: String -> Maybe Int
parseHeightToken raw =
  let tok = trim raw
   in readMaybe tok <|> parseNoteOctave tok
  where
    parseNoteOctave s =
      let (notePart, octPart) = break (\c -> isDigit c || c == '-') s
       in if null notePart || null octPart
            then Nothing
            else do
              pc  <- parsePitchClassName notePart
              oct <- readMaybe octPart
              return ((oct + 1) * 12 + pc)

-- | Parses a comma-separated sequence of pitches or contour ranks into @(labels, heights)@.
parseContourInput :: String -> Maybe ([String], [Int])
parseContourInput str =
  let parts = map trim (splitCommas str)
   in if length parts < 2 || any null parts
        then Nothing
        else do
          vals <- traverse parseHeightToken parts
          return (parts, vals)

-- | Converts any list of integer pitch heights into a normalized Contour Segment (CSeg)
-- where the lowest distinct pitch is ranked 0, next lowest 1, etc. (preserving ties).
toCSeg :: [Int] -> ContourSegment
toCSeg vals =
  let sortedUniq = sort (nub vals)
      rankOf v = case elemIndex v sortedUniq of
                   Just idx -> idx
                   Nothing  -> 0
   in map rankOf vals

-- | Retrograde of a contour segment: reverses temporal order.
contourRetrograde :: ContourSegment -> ContourSegment
contourRetrograde = reverse

-- | Inversion of a contour segment: flips vertical height (@maxRank - c@).
contourInversion :: ContourSegment -> ContourSegment
contourInversion []   = []
contourInversion cseg =
  let hi = maximum cseg
   in map (hi -) cseg

-- | Retrograde-Inversion of a contour segment.
contourRI :: ContourSegment -> ContourSegment
contourRI = contourRetrograde . contourInversion

-- | Computes the Prime Contour (lexicographically smallest form among P, R, I, RI).
primeContour :: ContourSegment -> ContourSegment
primeContour cseg =
  let p  = toCSeg cseg
      r  = contourRetrograde p
      i  = contourInversion p
      ri = contourRI p
   in minimum [p, r, i, ri]

-- | Builds the 2D Comparison Matrix (COM) for a contour segment:
-- entry (row i, col j) is '+' if c[j] > c[i], '-' if c[j] < c[i], and '0' if equal.
comMatrix :: ContourSegment -> [[Char]]
comMatrix cseg =
  [ [ cmpSymbol ci cj | cj <- cseg ] | ci <- cseg ]
  where
    cmpSymbol a b
      | b > a     = '+'
      | b < a     = '-'
      | otherwise = '0'

-- | Performs a complete Morris contour analysis on a sequence of pitches.
analyzeContour :: [String] -> [Int] -> ContourAnalysis
analyzeContour labels vals =
  let p  = toCSeg vals
      r  = contourRetrograde p
      i  = contourInversion p
      ri = contourRI p
      pr = minimum [p, r, i, ri]
      syms = [ lbl | (lbl, form) <- [("P", p), ("R", r), ("I", i), ("RI", ri)], form == p ]
   in ContourAnalysis
        { caInputLabels = labels
        , caRawValues   = vals
        , caCSeg        = p
        , caRetrograde  = r
        , caInversion   = i
        , caRI          = ri
        , caPrime       = pr
        , caSymmetry    = syms
        , caCOM         = comMatrix p
        }

-- | Checks whether two pitch sequences belong to the same Contour Class (related by P, R, I, or RI).
areContoursEquivalent :: [Int] -> [Int] -> Bool
areContoursEquivalent a b =
  length a == length b && primeContour a == primeContour b
