{-|
Module      : Neotonal
Description : Pitch-class set to neotonal chord vocabulary matcher and voice-leading neighbor finder
Stability   : experimental

Bridges post-tonal pitch-class set theory and neotonal harmony by matching
arbitrary pitch-class sets against rooted triads, seventh chords, extended/altered
chords, suspended chords, and quartal voicings via exact set equality,
subset/superset inclusion, and parsimonious L1 semitone distance.
-}
module Neotonal
  ( ChordTemplate(..)
  , ChordMatch(..)
  , NeotonalReport(..)
  , chordVocabulary
  , allRootedChords
  , analyzeNeotonal
  ) where

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import PitchClassSet
  ( PCSet
  , PitchClass
  , bestBijection
  , forteName
  , makeSet
  )
import Polytonal (pcName)

data ChordTemplate = ChordTemplate
  { ctSuffix    :: String -- ^ e.g. "maj7", "m7b5", "7alt", "quartal4"
  , ctIntervals :: [Int]  -- ^ Root-relative intervals mod 12
  , ctTag       :: String -- ^ Neutral descriptor tag: "Triad", "7th", "Extended", "Altered", "Suspended", "Quartal"
  } deriving (Show, Eq)

data ChordMatch = ChordMatch
  { cmSymbol    :: String -- ^ e.g. "B m7b5", "F 7#11"
  , cmTag       :: String -- ^ Descriptor tag from ChordTemplate
  , cmPitches   :: PCSet  -- ^ Concrete pitch classes of the chord
  , cmForte     :: String -- ^ Forte set-class name
  , cmExtraInfo :: String -- ^ Added/omitted tones or semitone alteration details
  , cmDistance  :: Int    -- ^ 0 for exact/subset/superset; L1 semitone distance for neighbors
  } deriving (Show, Eq)

data NeotonalReport = NeotonalReport
  { nrInputSet   :: PCSet
  , nrExact      :: [ChordMatch] -- ^ Rooted chords whose PCSet == S
  , nrSubsets    :: [ChordMatch] -- ^ Core chords embedded inside S (C subset S)
  , nrSupersets  :: [ChordMatch] -- ^ Extended chords containing S (S subset C)
  , nrNeighbors  :: [ChordMatch] -- ^ Equal-cardinality chords at L1 voice-leading distance 1
  } deriving (Show, Eq)

-- | Comprehensive dictionary of triads, seventh chords, extended, altered, suspended, and quartal chords.
chordVocabulary :: [ChordTemplate]
chordVocabulary =
  -- Triads
  [ ChordTemplate "maj"      (0 : 4 : 7 : mempty)             "Triad"
  , ChordTemplate "min"      (0 : 3 : 7 : mempty)             "Triad"
  , ChordTemplate "dim"      (0 : 3 : 6 : mempty)             "Diminished"
  , ChordTemplate "aug"      (0 : 4 : 8 : mempty)             "Augmented"
  , ChordTemplate "5(b5)"    (0 : 6 : 7 : mempty)             "Dyad/Cluster"
  -- Suspended
  , ChordTemplate "sus2"     (0 : 2 : 7 : mempty)             "Suspended"
  , ChordTemplate "sus4"     (0 : 5 : 7 : mempty)             "Suspended"
  , ChordTemplate "7sus4"    (0 : 5 : 7 : 10 : mempty)        "Suspended"
  , ChordTemplate "7sus4(b9)"(0 : 1 : 5 : 7 : 10 : mempty)    "Suspended/Dark"
  -- Quartal
  , ChordTemplate "quartal3" (0 : 5 : 10 : mempty)            "Quartal"
  , ChordTemplate "quartal4" (0 : 5 : 10 : 3 : mempty)        "Quartal"
  , ChordTemplate "quartal5" (0 : 5 : 10 : 3 : 8 : mempty)    "Quartal"
  -- Seventh Chords
  , ChordTemplate "maj7"     (0 : 4 : 7 : 11 : mempty)        "7th"
  , ChordTemplate "7"        (0 : 4 : 7 : 10 : mempty)        "7th"
  , ChordTemplate "m7"       (0 : 3 : 7 : 10 : mempty)        "7th"
  , ChordTemplate "m(maj7)"  (0 : 3 : 7 : 11 : mempty)        "7th/Dark"
  , ChordTemplate "m7b5"     (0 : 3 : 6 : 10 : mempty)        "Half-Dim"
  , ChordTemplate "dim7"     (0 : 3 : 6 : 9 : mempty)         "Diminished"
  , ChordTemplate "maj7#5"   (0 : 4 : 8 : 11 : mempty)        "Augmented"
  , ChordTemplate "7b5"      (0 : 4 : 6 : 10 : mempty)        "Altered"
  -- Extended & Altered
  , ChordTemplate "add9"     (0 : 2 : 4 : 7 : mempty)         "Extended"
  , ChordTemplate "m(add9)"  (0 : 2 : 3 : 7 : mempty)         "Extended"
  , ChordTemplate "6/9"      (0 : 2 : 4 : 7 : 9 : mempty)     "Extended"
  , ChordTemplate "9"        (0 : 2 : 4 : 7 : 10 : mempty)    "Extended"
  , ChordTemplate "maj9"     (0 : 2 : 4 : 7 : 11 : mempty)    "Extended"
  , ChordTemplate "m9"       (0 : 2 : 3 : 7 : 10 : mempty)    "Extended"
  , ChordTemplate "7b9"      (0 : 1 : 4 : 7 : 10 : mempty)    "Altered/Dark"
  , ChordTemplate "7#9"      (0 : 3 : 4 : 7 : 10 : mempty)    "Altered"
  , ChordTemplate "7#11"     (0 : 4 : 6 : 7 : 10 : mempty)    "Altered"
  , ChordTemplate "7alt"     (0 : 1 : 3 : 4 : 6 : 8 : 10 : mempty) "Altered/Dark"
  , ChordTemplate "maj7#11"  (0 : 4 : 6 : 7 : 11 : mempty)    "Lydian"
  , ChordTemplate "m11"      (0 : 2 : 3 : 5 : 7 : 10 : mempty)"Extended"
  , ChordTemplate "m(b6)"    (0 : 3 : 7 : 8 : mempty)         "Dark/Aeolian"
  , ChordTemplate "phryg"    (0 : 1 : 5 : 7 : mempty)         "Dark/Phrygian"
  ]

-- | Generates all rooted chord instances across all 12 pitch classes (C through B).
allRootedChords :: [(PitchClass, ChordTemplate, PCSet)]
allRootedChords =
  [ (r, tmpl, makeSet [ (r + iv) `mod` 12 | iv <- ctIntervals tmpl ])
  | r <- enumFromTo 0 11
  , tmpl <- chordVocabulary
  ]

formatNoteList :: PCSet -> String
formatNoteList xs = "[" ++ intercalate "," (map pcName xs) ++ "]"

-- | Analyzes an arbitrary pitch-class set against the neotonal chord vocabulary:
-- 1. Exact matches (C == S)
-- 2. Embedded core chords (C subset S, sorted by largest subset first)
-- 3. Extension supersets (S subset C, sorted by fewest added tones first)
-- 4. 1-semitone voice-leading neighbors (|C| == |S| and L1 distance == 1)
analyzeNeotonal :: PCSet -> NeotonalReport
analyzeNeotonal raw =
  let s = makeSet raw
      cardS = length s
      catalog = allRootedChords

      exactMatches =
        [ ChordMatch
            { cmSymbol    = pcName r ++ " " ++ ctSuffix tmpl
            , cmTag       = ctTag tmpl
            , cmPitches   = cPcs
            , cmForte     = forteName cPcs
            , cmExtraInfo = "Exact pitch-class match"
            , cmDistance  = 0
            }
        | (r, tmpl, cPcs) <- catalog
        , cPcs == s
        ]

      subsetMatches =
        sortBy (comparing (negate . length . cmPitches))
          [ ChordMatch
              { cmSymbol    = pcName r ++ " " ++ ctSuffix tmpl
              , cmTag       = ctTag tmpl
              , cmPitches   = cPcs
              , cmForte     = forteName cPcs
              , cmExtraInfo = "Non-chord tones in set: " ++ formatNoteList nonChord
              , cmDistance  = 0
              }
          | (r, tmpl, cPcs) <- catalog
          , length cPcs < cardS
          , all (`elem` s) cPcs
          , let nonChord = [ x | x <- s, x `notElem` cPcs ]
          ]

      supersetMatches =
        sortBy (comparing (length . cmPitches))
          [ ChordMatch
              { cmSymbol    = pcName r ++ " " ++ ctSuffix tmpl
              , cmTag       = ctTag tmpl
              , cmPitches   = cPcs
              , cmForte     = forteName cPcs
              , cmExtraInfo = "Completing tones needed: " ++ formatNoteList missing
              , cmDistance  = 0
              }
          | (r, tmpl, cPcs) <- catalog
          , length cPcs > cardS
          , all (`elem` cPcs) s
          , let missing = [ x | x <- cPcs, x `notElem` s ]
          ]

      neighborMatches =
        if cardS == 0
          then mempty
          else
            [ ChordMatch
                { cmSymbol    = pcName r ++ " " ++ ctSuffix tmpl
                , cmTag       = ctTag tmpl
                , cmPitches   = cPcs
                , cmForte     = forteName cPcs
                , cmExtraInfo = "1-semitone shift: " ++ fmtDiff pairing
                , cmDistance  = 1
                }
            | (r, tmpl, cPcs) <- catalog
            , length cPcs == cardS
            , cPcs /= s
            , let (dist, pairing) = bestBijection s cPcs
            , dist == 1
            ]
   in NeotonalReport
        { nrInputSet  = s
        , nrExact     = exactMatches
        , nrSubsets   = subsetMatches
        , nrSupersets = supersetMatches
        , nrNeighbors = neighborMatches
        }
  where
    fmtDiff pairing =
      intercalate ", "
        [ pcName a ++ "->" ++ pcName b ++ " (" ++ (if d > 0 then "+" else "") ++ show d ++ ")"
        | (a, b, d) <- pairing
        , d /= 0
        ]
