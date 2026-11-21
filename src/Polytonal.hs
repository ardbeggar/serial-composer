{-|
Module      : Polytonal
Description : Polytonal / bitonal ambiguity, scale-collapse, and accidental-resolution analyzer
Stability   : experimental

Analyzes two superimposed tonal layers (scales, modes, or polychords) to
measure bitonal friction, detect collapse into a single parent diatonic
scale, and identify accidental dominant-seventh (4-27) resolutions to avoid.
-}
module Polytonal
  ( ScaleMode(..)
  , PolyLayer(..)
  , DominantTrap(..)
  , PolyAnalysis(..)
  , pcName
  , modeIntervals
  , buildLayer
  , parseLayerSpec
  , analyzeBitonal
  , StepHazard(..)
  , PolyStepReport(..)
  , evaluatePolyStep
  ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toLower, toUpper)
import Data.List (elemIndex, intercalate, nub)
import PitchClassSet
  ( IntervalVector
  , PCSet
  , PitchClass
  , forteName
  , intervalVector
  , makeSet
  , primeForm
  )
import Text.Read (readMaybe)

data ScaleMode
  = ModeMajor
  | ModeNaturalMinor
  | ModeHarmonicMinor
  | ModeMelodicMinor
  | ModeDorian
  | ModePhrygian
  | ModeLydian
  | ModeMixolydian
  | ModeLocrian
  | ChordMajorTriad
  | ChordMinorTriad
  | ChordDimTriad
  | ChordAugTriad
  | ChordDom7
  | ChordMaj7
  | ChordMin7
  deriving (Show, Eq)

data PolyLayer = PolyLayer
  { plLabel   :: String
  , plRoot    :: Maybe PitchClass
  , plPitches :: PCSet
  } deriving (Show, Eq)

data DominantTrap = DominantTrap
  { dtDomRoot     :: PitchClass -- ^ Root of accidental V7 chord
  , dtDomPitches  :: PCSet      -- ^ [r, r+4, r+7, r+10]
  , dtTargetTonic :: PitchClass -- ^ Tonic (r + 5) mod 12 that V7 wants to resolve into
  , dtTritone     :: (PitchClass, PitchClass) -- ^ (3rd = r+4, 7th = r+10) resolution pair
  } deriving (Show, Eq)

data PolyAnalysis = PolyAnalysis
  { paLayerA          :: PolyLayer
  , paLayerB          :: PolyLayer
  , paUnion           :: PCSet
  , paUnionForte      :: String
  , paUnionPrime      :: PCSet
  , paUnionVector     :: IntervalVector
  , paSharedPitches   :: PCSet
  , paCrossHalfSteps  :: [(PitchClass, PitchClass)] -- ^ (a in A, b in B) with |a-b| == 1 mod 12
  , paCrossTritones   :: [(PitchClass, PitchClass)] -- ^ (a in A, b in B) with |a-b| == 6 mod 12
  , paParentScaleHits :: [String]       -- ^ Single 7-note tonal scales containing A U B
  , paDominantTraps   :: [DominantTrap] -- ^ Embedded V7 chords pulling toward a tonic
  , paAmbiguityScore  :: Int            -- ^ Heuristic score (0-100) for sustained bitonal independence
  , paVerdict         :: String
  } deriving (Show, Eq)

pcName :: PitchClass -> String
pcName pc =
  let names = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]
   in names !! (pc `mod` 12)

modeIntervals :: ScaleMode -> [Int]
modeIntervals ModeMajor         = makeSet [0, 2, 4, 5, 7, 9, 11]
modeIntervals ModeNaturalMinor  = makeSet [0, 2, 3, 5, 7, 8, 10]
modeIntervals ModeHarmonicMinor = makeSet [0, 2, 3, 5, 7, 8, 11]
modeIntervals ModeMelodicMinor  = makeSet [0, 2, 3, 5, 7, 9, 11]
modeIntervals ModeDorian        = makeSet [0, 2, 3, 5, 7, 9, 10]
modeIntervals ModePhrygian      = makeSet [0, 1, 3, 5, 7, 8, 10]
modeIntervals ModeLydian        = makeSet [0, 2, 4, 6, 7, 9, 11]
modeIntervals ModeMixolydian    = makeSet [0, 2, 4, 5, 7, 9, 10]
modeIntervals ModeLocrian       = makeSet [0, 1, 3, 5, 6, 8, 10]
modeIntervals ChordMajorTriad   = makeSet [0, 4, 7]
modeIntervals ChordMinorTriad   = makeSet [0, 3, 7]
modeIntervals ChordDimTriad     = makeSet [0, 3, 6]
modeIntervals ChordAugTriad     = makeSet [0, 4, 8]
modeIntervals ChordDom7         = makeSet [0, 4, 7, 10]
modeIntervals ChordMaj7         = makeSet [0, 4, 7, 11]
modeIntervals ChordMin7         = makeSet [0, 3, 7, 10]

buildLayer :: String -> PitchClass -> ScaleMode -> PolyLayer
buildLayer lbl root mode =
  let pcs = makeSet [ (root + iv) `mod` 12 | iv <- modeIntervals mode ]
   in PolyLayer
        { plLabel   = lbl
        , plRoot    = Just (root `mod` 12)
        , plPitches = pcs
        }

--------------------------------------------------------------------------------
-- Parsing Tonal Layers ("B:phrygian", "F:min", or "0,4,7")
--------------------------------------------------------------------------------

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseRoot :: String -> Maybe PitchClass
parseRoot str =
  let norm = map toUpper (trim str)
      flats = ["C", "DB", "D", "EB", "E", "F", "GB", "G", "AB", "A", "BB", "B"]
      sharps = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in elemIndex norm flats <|> elemIndex norm sharps

parseModeName :: String -> Maybe (String, ScaleMode)
parseModeName str =
  case map toLower (trim str) of
    "major"         -> Just ("Major", ModeMajor)
    "maj"           -> Just ("Major Triad", ChordMajorTriad)
    "ionian"        -> Just ("Ionian", ModeMajor)
    "minor"         -> Just ("Natural Minor", ModeNaturalMinor)
    "aeolian"       -> Just ("Aeolian", ModeNaturalMinor)
    "min"           -> Just ("Minor Triad", ChordMinorTriad)
    "m"             -> Just ("Minor Triad", ChordMinorTriad)
    "harmonicminor" -> Just ("Harmonic Minor", ModeHarmonicMinor)
    "harmmin"       -> Just ("Harmonic Minor", ModeHarmonicMinor)
    "melodicminor"  -> Just ("Melodic Minor", ModeMelodicMinor)
    "melmin"        -> Just ("Melodic Minor", ModeMelodicMinor)
    "dorian"        -> Just ("Dorian", ModeDorian)
    "phrygian"      -> Just ("Phrygian", ModePhrygian)
    "lydian"        -> Just ("Lydian", ModeLydian)
    "mixolydian"    -> Just ("Mixolydian", ModeMixolydian)
    "locrian"       -> Just ("Locrian", ModeLocrian)
    "dim"           -> Just ("Diminished Triad", ChordDimTriad)
    "aug"           -> Just ("Augmented Triad", ChordAugTriad)
    "dom7"          -> Just ("Dominant 7th", ChordDom7)
    "7"             -> Just ("Dominant 7th", ChordDom7)
    "maj7"          -> Just ("Major 7th", ChordMaj7)
    "min7"          -> Just ("Minor 7th", ChordMin7)
    "m7"            -> Just ("Minor 7th", ChordMin7)
    _               -> Nothing

splitCommas :: String -> [String]
splitCommas s =
  case break (== ',') s of
    (chunk, "")       -> [chunk]
    (chunk, _ : rest) -> chunk : splitCommas rest

parsePitchToken :: String -> Maybe PitchClass
parsePitchToken tok =
  let clean = trim tok
   in readMaybe clean <|> parseRoot clean

-- | Parses either a named tonal layer (@"B:phrygian"@, @"F:min"@, @"C:dom7"@)
-- or a comma-separated set of pitch classes (@"11,2,6"@ or @"B,D,F#"@).
parseLayerSpec :: String -> Maybe PolyLayer
parseLayerSpec raw =
  let s = trim raw
   in case break (== ':') s of
        (rootPart, ':' : modePart) -> do
          r <- parseRoot rootPart
          (mLabel, mode) <- parseModeName modePart
          return $ buildLayer (pcName r ++ " " ++ mLabel) r mode
        _ -> do
          let parts = splitCommas s
          if null parts || any (null . trim) parts
            then Nothing
            else do
              pcs <- makeSet <$> traverse parsePitchToken parts
              return PolyLayer
                { plLabel   = "[" ++ intercalate "," (map pcName pcs) ++ "]"
                , plRoot    = Nothing
                , plPitches = pcs
                }

--------------------------------------------------------------------------------
-- Bitonal Ambiguity, Scale-Collapse, and Dominant-Trap Detection
--------------------------------------------------------------------------------

-- | Checks whether @unionSet@ is completely contained inside any single standard
-- 7-note tonal scale (Major/Diatonic, Melodic Minor, Harmonic Minor, Harmonic Major).
findParentTonalScales :: PCSet -> [String]
findParentTonalScales unionSet =
  let families =
        [ ("Diatonic / Major", ModeMajor)
        , ("Melodic Minor",    ModeMelodicMinor)
        , ("Harmonic Minor",   ModeHarmonicMinor)
        ]
   in [ pcName r ++ " " ++ famName
      | (famName, mode) <- families
      , r <- enumFromTo 0 11
      , let scalePcs = [ (r + iv) `mod` 12 | iv <- modeIntervals mode ]
      , all (`elem` scalePcs) unionSet
      ]

-- | Scans @unionSet@ for embedded dominant-seventh chords @[r, r+4, r+7, r+10]@
-- that span across both layers (not already contained purely inside one layer).
findDominantTraps :: PCSet -> PCSet -> PCSet -> [DominantTrap]
findDominantTraps setA setB unionSet =
  [ DominantTrap
      { dtDomRoot     = r
      , dtDomPitches  = domPcs
      , dtTargetTonic = (r + 5) `mod` 12
      , dtTritone     = ((r + 4) `mod` 12, (r + 10) `mod` 12)
      }
  | r <- enumFromTo 0 11
  , let domPcs = makeSet [ r, r + 4, r + 7, r + 10 ]
  , all (`elem` unionSet) domPcs
  , not (all (`elem` setA) domPcs)
  , not (all (`elem` setB) domPcs)
  ]

-- | Performs a full polytonal ambiguity and anti-resolution analysis on two layers.
analyzeBitonal :: PolyLayer -> PolyLayer -> PolyAnalysis
analyzeBitonal layA layB =
  let setA = plPitches layA
      setB = plPitches layB
      uSet = makeSet (setA ++ setB)
      uPf  = primeForm uSet
      shared = [ x | x <- setA, x `elem` setB ]
      halfSteps =
        [ (a, b)
        | a <- setA
        , b <- setB
        , let d = (b - a) `mod` 12
        , d == 1 || d == 11
        ]
      tritones =
        [ (a, b)
        | a <- setA
        , b <- setB
        , (b - a) `mod` 12 == 6
        ]
      parents = findParentTonalScales uSet
      traps   = findDominantTraps setA setB uSet
      -- Heuristic ambiguity score: rewards cross-layer ic1/ic6 friction, penalizes single-scale collapse & V7 traps
      rawScore = 50
               + (length halfSteps * 8)
               + (length tritones * 6)
               - (length shared * 5)
               - (if null parents then 0 else 30)
               - (length traps * 15)
      clamped = max 0 (min 100 rawScore)
      verdict
        | setA == setB =
            "Identical pitch collections (0% bitonal separation — pure unison/monotonal)."
        | not (null parents) =
            "Single-Key Collapse: Combined notes fit inside one tonal scale ("
            ++ intercalate ", " (take 3 parents)
            ++ "); ear will hear extended single-key harmony."
        | clamped >= 70 && null traps =
            "High Polytonal Independence: Strong cross-layer friction with no accidental V7 pull."
        | clamped >= 50 =
            "Moderate Bitonal Tension: Distinct tonal centers, but watch highlighted V7/shared pivots."
        | otherwise =
            "Low Ambiguity / High Resolution Pull: Strong overlap or dominant-7th gravity."
   in PolyAnalysis
        { paLayerA          = layA
        , paLayerB          = layB
        , paUnion           = uSet
        , paUnionForte      = forteName uSet
        , paUnionPrime      = uPf
        , paUnionVector     = intervalVector uPf
        , paSharedPitches   = shared
        , paCrossHalfSteps  = halfSteps
        , paCrossTritones   = tritones
        , paParentScaleHits = parents
        , paDominantTraps   = traps
        , paAmbiguityScore  = clamped
        , paVerdict         = verdict
        }

--------------------------------------------------------------------------------
-- Polytonal Progression & Anti-Resolution Step Validator
--------------------------------------------------------------------------------

data StepHazard = StepHazard
  { shCategory    :: String -- ^ e.g. "Tritone Resolution", "Leading-Tone Cadence", "Parallel Root Lock", "Tonal Collapse"
  , shDescription :: String -- ^ Detailed musical explanation and pitches involved
  } deriving (Show, Eq)

data PolyStepReport = PolyStepReport
  { psState1       :: PolyAnalysis
  , psState2       :: PolyAnalysis
  , psCommonTones  :: PCSet        -- ^ Pitches held stationary from (A1 U B1) to (A2 U B2)
  , psHazards      :: [StepHazard] -- ^ Detected resolution or absorption hazards
  , psPreservesAmb :: Bool         -- ^ True if zero hazards and State 2 maintains ambiguity >= 50
  } deriving (Show, Eq)

-- | Evaluates a progression step @(layA1, layB1) -> (layA2, layB2)@ for accidental
-- tonal resolutions, tritone discharges, parallel root locking, and single-key collapse.
evaluatePolyStep :: (PolyLayer, PolyLayer) -> (PolyLayer, PolyLayer) -> PolyStepReport
evaluatePolyStep (a1, b1) (a2, b2) =
  let an1 = analyzeBitonal a1 b1
      an2 = analyzeBitonal a2 b2
      u1  = paUnion an1
      u2  = paUnion an2
      held = [ x | x <- u1, x `elem` u2 ]

      -- 1. Check all tritones in State 1 for contrary-motion semitone resolution into State 2
      tritones1 = [ (x, y) | x <- u1, y <- u1, x < y, (y - x) `mod` 12 == 6 ]
      tritoneHazards =
        [ StepHazard
            { shCategory = "Tritone Resolution"
            , shDescription =
                "Tritone " ++ pcName x ++ "-" ++ pcName y
                ++ " in State 1 resolves by semitone to "
                ++ pcName rx ++ "-" ++ pcName ry
                ++ " in State 2 (triggers V7 -> I tonal gravity)."
            }
        | (x, y) <- tritones1
        , (rx, ry) <- [ ((x + 1) `mod` 12, (y - 1) `mod` 12)
                      , ((x - 1) `mod` 12, (y + 1) `mod` 12)
                      ]
        , rx `elem` u2 && ry `elem` u2
        ]

      -- 2. Check leading-tone (+1 semitone) resolution into State 2 layer roots
      targetRoots = nub [ r | Just r <- [plRoot a2, plRoot b2] ]
      ltHazards =
        [ StepHazard
            { shCategory = "Leading-Tone Cadence"
            , shDescription =
                "Leading tone " ++ pcName lt ++ " in State 1 resolves up by half-step to root "
                ++ pcName r ++ " in State 2."
            }
        | r <- targetRoots
        , let lt = (r - 1) `mod` 12
        , lt `elem` u1 && r `elem` u2 && lt `notElem` u2
        ]

      -- 3. Check parallel fifths/octaves/unisons between explicit layer roots
      parallelHazards =
        case (plRoot a1, plRoot b1, plRoot a2, plRoot b2) of
          (Just rA1, Just rB1, Just rA2, Just rB2) ->
            let span1 = (rB1 - rA1) `mod` 12
                span2 = (rB2 - rA2) `mod` 12
                moveA = (rA2 - rA1) `mod` 12
                moveB = (rB2 - rB1) `mod` 12
             in [ StepHazard
                    { shCategory = "Parallel Layer Lock"
                    , shDescription =
                        "Both layer roots move in parallel (" ++ pcName rA1 ++ "->" ++ pcName rA2
                        ++ " and " ++ pcName rB1 ++ "->" ++ pcName rB2
                        ++ ") at a consonant interval (" ++ show span1 ++ " semitones), fusing two keys into one stream."
                    }
                | moveA /= 0 && moveA == moveB && span1 == span2 && (span1 == 0 || span1 == 5 || span1 == 7)
                ]
          _ -> mempty

      -- 4. Check if State 2 collapses into a single triad or single diatonic/minor scale
      collapseHazards =
        [ StepHazard
            { shCategory = "Single-Triad Resolution"
            , shDescription =
                "State 2 collapses into a single consonant triad (" ++ paUnionForte an2
                ++ "), terminating polytonal tension."
            }
        | paUnionForte an2 == "3-11"
        ]
        ++
        [ StepHazard
            { shCategory = "Single-Scale Absorption"
            , shDescription =
                "State 2 falls completely inside one tonal scale ("
                ++ intercalate ", " (take 2 (paParentScaleHits an2)) ++ ")."
            }
        | not (null (paParentScaleHits an2))
        ]

      allHazards = tritoneHazards ++ ltHazards ++ parallelHazards ++ collapseHazards
      okAmb = null allHazards && paAmbiguityScore an2 >= 50
   in PolyStepReport
        { psState1       = an1
        , psState2       = an2
        , psCommonTones  = held
        , psHazards      = allHazards
        , psPreservesAmb = okAmb
        }
