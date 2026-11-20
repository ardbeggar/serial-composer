{-|
Module      : SerialComposer
Description : N x N serial matrix generator, groove realizer, and score exporter
Stability   : experimental

Generates generalized @N x N@ serial matrices (12-tone or smaller pitch-class
sets), extracts @P@, @R@, @I@, and @RI@ transformations, realizes rhythmically
displaced multi-part scores, performs Forte set-class row segmentation, and
exports to MusicXML and LilyPond.
-}

module SerialComposer
  ( PitchClass
  , MidiPitch
  , Row
  , Matrix
  , NoteEvent(..)
  , PartSpec(..)
  , SegmentInfo(..)
  , deriveRow12
  , allIntervalRow12
  , partitionRow
  , slidingSegments
  , analyzeSegments
  , printRowSegmentation
  , randomRow
  , generateMatrix
  , getPrime
  , getRetrograde
  , getInversion
  , getRetrogradeInversion
  , pcToNoteName
  , printMatrix
  , printNoteMatrix
  , realizePlain
  , realizeGroove
  , realizeSectionsGroove
  , exportLilyPondParts
  , exportMusicXMLParts

  , InvarianceMatch(..)
  , allRowForms
  , scanRowInvariance
  , printRowInvariance

  , multiplyRow
  , m5Row
  , m7Row

  , rotationalArray
  , rotationalVerticals
  , printRotationalArray
  ) where

import Prelude hiding (span)
import Data.List (sortBy, intercalate, nub, sort, transpose)
import Data.Ord (comparing)
import System.Random (StdGen, randomR, randoms, split)
import Text.Printf (printf)
import qualified PitchClassSet as PCS

type PitchClass = Int
type MidiPitch = Int
type Row = [PitchClass]
type Matrix = [Row]

-- | Represents a realized musical event.
-- evDuration: measured in eighth-note ticks (1 = eighth, 2 = quarter, 3 = dotted quarter, 4 = half)
data NoteEvent = NoteEvent
  { evMidi     :: MidiPitch
  , evDuration :: Int
  , evIsRest   :: Bool
  } deriving (Show, Eq)

-- | Defines a single instrument staff/part for export.
data PartSpec = PartSpec
  { partName       :: String
  , partClef       :: String -- "guitar" (treble_8) or "bass" (bass_8)
  , partInstrument :: String -- General MIDI name, e.g., "distorted guitar", "electric bass (pick)"
  , partSections   :: [(String, [NoteEvent])]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 1. Random Row & Matrix Generation
--------------------------------------------------------------------------------

-- | Purely generates a random non-repeating row of length @n@ from pitch classes [0 .. m-1].
randomRow :: Int -> Int -> StdGen -> (Row, StdGen)
randomRow m n gen
  | n > m = error "Row length cannot exceed modulus without duplicates"
  | n <= 0 = ([], gen)
  | otherwise =
      let (g1, nextGen) = split gen
          randWeights = take m (randoms g1 :: [Int])
          shuffled = map fst $ sortBy (comparing snd) (zip [0 .. m - 1] randWeights)
       in (take n shuffled, nextGen)

-- | Generates an N x N matrix from an initial prime row (P0) and modulus m.
generateMatrix :: Int -> Row -> Matrix
generateMatrix _ [] = []
generateMatrix m p0@(first : _) =
  let normP0 = map (\x -> (x - first) `mod` m) p0
      i0 = map (\x -> (-x) `mod` m) normP0
   in [map (\p -> (i + p) `mod` m) normP0 | i <- i0]

-- | Applies an ordered multiplicative transform @x -> (k * x) mod m@ to a row.
multiplyRow :: Int -> Int -> [PitchClass] -> [PitchClass]
multiplyRow m k = map (\x -> (k * x) `mod` m)

-- | Transforms an ordered row via M5 (cycle of fourths, @x -> 5x mod m@).
m5Row :: Int -> [PitchClass] -> [PitchClass]
m5Row m = multiplyRow m 5

-- | Transforms an ordered row via M7 (cycle of fifths, @x -> 7x mod m@).
m7Row :: Int -> [PitchClass] -> [PitchClass]
m7Row m = multiplyRow m 7

getPrime, getRetrograde, getInversion, getRetrogradeInversion :: Matrix -> Int -> Row
getPrime mat idx = mat !! idx
getRetrograde mat idx = reverse (mat !! idx)
getInversion mat idx = map (!! idx) mat
getRetrogradeInversion mat idx = reverse $ map (!! idx) mat

--------------------------------------------------------------------------------
-- 2. Console Pretty-Printers
--------------------------------------------------------------------------------

pcToNoteName :: PitchClass -> String
pcToNoteName pc =
  ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"] !! (pc `mod` 12)

-- | Prints the labeled integer matrix with P, I, R, and RI headers.
printMatrix :: Matrix -> IO ()
printMatrix [] = putStrLn "(empty matrix)"
printMatrix mat@(p0 : _) = do
  let i0 = [r | (r : _) <- mat]
      topHeader = "      " ++ unwords (map (printf " I%-2d") i0)
      botFooter = "      " ++ unwords (map (printf "RI%-2d") i0)
      divider = "     +" ++ replicate (length p0 * 4 + 1) '-' ++ "+"

      formatRow :: (Int, Row) -> String
      formatRow (idx, row) =
        let label = i0 !! idx
            cells = unwords $ map (printf "%3d") row
         in printf "P%-3d | %s | R%-2d" label cells label

  putStrLn topHeader
  putStrLn divider
  mapM_ (putStrLn . formatRow) (zip [0 ..] mat)
  putStrLn divider
  putStrLn botFooter

-- | Prints the matrix directly as note names transposed to @rootOffset@ (0=C, 2=D, 11=B).
printNoteMatrix :: PitchClass -> Matrix -> IO ()
printNoteMatrix _ [] = putStrLn "(empty matrix)"
printNoteMatrix rootOffset mat@(p0 : _) = do
  let i0 = [r | (r : _) <- mat]
      toNote pc = pcToNoteName ((pc + rootOffset) `mod` 12)
      topHeader = "      " ++ unwords (map (printf " I%-2d") i0)
      botFooter = "      " ++ unwords (map (printf "RI%-2d") i0)
      divider = "     +" ++ replicate (length p0 * 4 + 1) '-' ++ "+"

      formatRow :: (Int, Row) -> String
      formatRow (idx, row) =
        let label = i0 !! idx
            cells = unwords $ map (printf "%3s" . toNote) row
         in printf "P%-3d | %s | R%-2d" label cells label

  putStrLn topHeader
  putStrLn divider
  mapM_ (putStrLn . formatRow) (zip [0 ..] mat)
  putStrLn divider
  putStrLn botFooter

--------------------------------------------------------------------------------
-- 3. Phrase Realization (Plain vs. Rhythm + Octave Displacement)
--------------------------------------------------------------------------------

toBaseMidi :: PitchClass -> Int -> PitchClass -> MidiPitch
toBaseMidi rootOffset baseOctave pc =
  (baseOctave + 1) * 12 + (rootOffset `mod` 12) + pc

-- | Realizes a row as plain, un-displaced quarter notes (2 eighth-note ticks each).
realizePlain :: PitchClass -> Int -> Row -> [NoteEvent]
realizePlain rootOffset baseOctave row =
  [NoteEvent (toBaseMidi rootOffset baseOctave pc) 2 False | pc <- row]

-- | Realizes a row with random octave displacement and heavy/syncopated rhythms.
realizeGroove :: PitchClass -> Int -> Int -> Int -> StdGen -> Row -> ([NoteEvent], StdGen)
realizeGroove _ _ _ _ gen [] = ([], gen)
realizeGroove rootOffset baseOctave octaveSpan restChance gen (pc : pcs) =
  let (octJump, g1) = randomR (0, octaveSpan) gen
      durPool = [1, 1, 2, 2, 2, 3, 4]
      (durIdx, g2) = randomR (0, length durPool - 1) g1
      dur = durPool !! durIdx
      midi = toBaseMidi rootOffset baseOctave pc + (octJump * 12)
      noteEv = NoteEvent midi dur False
      (roll, g3) = randomR (1, 100) g2
      (restEv, g4) =
        if roll <= restChance
          then ([NoteEvent 0 1 True], g3)
          else ([], g3)
      (tailEvs, finalGen) = realizeGroove rootOffset baseOctave octaveSpan restChance g4 pcs
   in (noteEv : restEv ++ tailEvs, finalGen)

-- | Realizes multiple labeled rows sequentially, threading the StdGen state.
realizeSectionsGroove ::
  PitchClass -> Int -> Int -> Int -> StdGen -> [(String, Row)] -> ([(String, [NoteEvent])], StdGen)
realizeSectionsGroove _ _ _ _ gen [] = ([], gen)
realizeSectionsGroove root oct span restPct gen ((lbl, r) : rest) =
  let (evs, g1) = realizeGroove root oct span restPct gen r
      (more, g2) = realizeSectionsGroove root oct span restPct g1 rest
   in ((lbl, evs) : more, g2)

--------------------------------------------------------------------------------
-- 4. LilyPond Multi-Part Export (.ly)
--------------------------------------------------------------------------------

midiToLilyPitch :: MidiPitch -> String
midiToLilyPitch midi =
  let pc = midi `mod` 12
      octave = (midi `div` 12) - 1
      noteName = ["c", "des", "d", "ees", "e", "f", "ges", "g", "aes", "a", "bes", "b"] !! pc
      octaveMark
        | octave >= 3 = replicate (octave - 3) '\''
        | otherwise = replicate (3 - octave) ','
   in noteName ++ octaveMark

ticksToLilyDur :: Int -> String
ticksToLilyDur 1 = "8"
ticksToLilyDur 2 = "4"
ticksToLilyDur 3 = "4."
ticksToLilyDur 4 = "2"
ticksToLilyDur n = show n

exportLilyPondParts :: FilePath -> Int -> [PartSpec] -> IO ()
exportLilyPondParts path bpm parts = writeFile path content
  where
    formatEvent clefShift ev
      | evIsRest ev = "r" ++ ticksToLilyDur (evDuration ev)
      | otherwise = midiToLilyPitch (evMidi ev + clefShift) ++ ticksToLilyDur (evDuration ev)

    formatStaff part =
      let (lilyClef, clefShift) =
            if partClef part == "bass"
              then ("bass_8", 12)
              else ("treble_8", 12)
          formatSec (lbl, evs) =
            printf "      \\mark \"%s\" %s \\bar \"||\"\n" lbl (unwords $ map (formatEvent clefShift) evs)
       in unlines
            [ printf
                "    \\new Staff \\with { instrumentName = \"%s\" midiInstrument = \"%s\" } {"
                (partName part)
                (partInstrument part)
            , printf "      \\clef \"%s\"" lilyClef
            , "      \\numericTimeSignature"
            , "      \\cadenzaOn"
            , concatMap formatSec (partSections part)
            , "    }"
            ]

    content =
      unlines
        [ "\\version \"2.24.0\""
        , "\\score {"
        , "  <<"
        , concatMap formatStaff parts
        , "  >>"
        , "  \\layout { }"
        , printf "  \\midi { \\tempo 4 = %d }" bpm
        , "}"
        ]

--------------------------------------------------------------------------------
-- 5. MusicXML Multi-Part Export (.musicxml -> MuseScore)
--------------------------------------------------------------------------------

midiToXmlPitch :: MidiPitch -> (String, Int, Int)
midiToXmlPitch midi =
  let pc = midi `mod` 12
      octave = (midi `div` 12) - 1
      (step, alter) =
        [ ("C", 0), ("D", -1), ("D", 0), ("E", -1)
        , ("E", 0), ("F", 0), ("G", -1), ("G", 0)
        , ("A", -1), ("A", 0), ("B", -1), ("B", 0)
        ] !! pc
   in (step, alter, octave)

ticksToXmlType :: Int -> (String, Bool)
ticksToXmlType 1 = ("eighth", False)
ticksToXmlType 2 = ("quarter", False)
ticksToXmlType 3 = ("quarter", True)
ticksToXmlType 4 = ("half", False)
ticksToXmlType _ = ("quarter", False)

exportMusicXMLParts :: FilePath -> [PartSpec] -> IO ()
exportMusicXMLParts path parts = writeFile path content
  where
    partIds = ["P" ++ show i | i <- [1 .. length parts]]

    formatPartList (pid, part) =
      printf "    <score-part id=\"%s\"><part-name>%s</part-name></score-part>\n" pid (partName part)

    formatEvent ev
      | evIsRest ev =
          let (tName, dotted) = ticksToXmlType (evDuration ev)
              dotTag = if dotted then "<dot/>" else ""
           in unlines
                [ "      <note><rest/>"
                , printf "        <duration>%d</duration><type>%s</type>%s" (evDuration ev) tName dotTag
                , "      </note>"
                ]
      | otherwise =
          let (step, alter, octave) = midiToXmlPitch (evMidi ev + 12)
              alterTag = if alter /= 0 then printf "<alter>%d</alter>" alter else ""
              (tName, dotted) = ticksToXmlType (evDuration ev)
              dotTag = if dotted then "<dot/>" else ""
           in unlines
                [ "      <note>"
                , "        <pitch>"
                , printf "          <step>%s</step>%s<octave>%d</octave>" step alterTag octave
                , "        </pitch>"
                , printf "        <duration>%d</duration><type>%s</type>%s" (evDuration ev) tName dotTag
                , "      </note>"
                ]

    formatPartBody (pid, part) =
      let clefXml =
            if partClef part == "bass"
              then "<clef><sign>F</sign><line>4</line><clef-octave-change>-1</clef-octave-change></clef>"
              else "<clef><sign>G</sign><line>2</line><clef-octave-change>-1</clef-octave-change></clef>"

          formatMeasure :: (Int, (String, [NoteEvent])) -> String
          formatMeasure (idx, (lbl, evs)) =
            unlines
              [ printf "    <measure number=\"%d\">" idx
              , if idx == 1
                  then printf "      <attributes><divisions>2</divisions><key><fifths>0</fifths></key>%s</attributes>" clefXml
                  else ""
              , printf "      <direction placement=\"above\"><direction-type><rehearsal>%s</rehearsal></direction-type></direction>" lbl
              , concatMap formatEvent evs
              , "    </measure>"
              ]
       in unlines
            [ printf "  <part id=\"%s\">" pid
            , concatMap formatMeasure (zip [1 ..] (partSections part))
            , "  </part>"
            ]

    content =
      unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.1 Partwise//EN\" \"http://musicxml.org\">"
        , "<score-partwise version=\"3.1\">"
        , "  <part-list>"
        , concatMap formatPartList (zip partIds parts)
        , "  </part-list>"
        , concatMap formatPartBody (zip partIds parts)
        , "</score-partwise>"
        ]

--------------------------------------------------------------------------------
-- Pitch-Class Set Segmentation Analysis (Mod 12)
--------------------------------------------------------------------------------

data SegmentInfo = SegmentInfo
  { siPitches   :: [PitchClass]
  , siForteName :: String
  , siPrimeForm :: PCS.PCSet
  , siVector    :: PCS.IntervalVector
  } deriving (Show, Eq)

-- | Splits a row into contiguous non-overlapping chunks of size @k@
-- (dropping any trailing remainder shorter than @k@).
partitionRow :: Int -> [PitchClass] -> [[PitchClass]]
partitionRow k xs
  | k <= 0        = mempty
  | length xs < k = mempty
  | otherwise     = take k xs : partitionRow k (drop k xs)

-- | Generates all contiguous overlapping windows of size @k@ across a row.
slidingSegments :: Int -> [PitchClass] -> [[PitchClass]]
slidingSegments k xs
  | k <= 0 || length xs < k = mempty
  | otherwise               = take k xs : slidingSegments k (drop 1 xs)

-- | Analyzes a list of pitch-class segments using Forte set theory.
analyzeSegments :: [[PitchClass]] -> [SegmentInfo]
analyzeSegments = map toInfo
  where
    toInfo seg =
      let pf = PCS.primeForm seg
       in SegmentInfo
            { siPitches   = seg
            , siForteName = PCS.forteName seg
            , siPrimeForm = pf
            , siVector    = PCS.intervalVector pf
            }

-- | Prints discrete and overlapping segment analyses for a row (when modulus == 12).
printRowSegmentation :: Int -> [PitchClass] -> IO ()
printRowSegmentation rootOffset row = do
  let n = length row
      transposed = map (\p -> (p + rootOffset) `mod` 12) row
      noteLabels = map pcToNoteName transposed
  putStrLn $ "\n=== Row Set-Class Segmentation (Root: " ++ pcToNoteName rootOffset ++ ") ==="
  putStrLn $ "P0 Pitches: " ++ unwords noteLabels
          ++ "  (" ++ intercalate "," (map show transposed) ++ ")"
  putStrLn $ "Whole Row:  Forte " ++ PCS.forteName row
          ++ " (" ++ intercalate "," (map show (PCS.primeForm row)) ++ ") "
          ++ PCS.formatVector (PCS.intervalVector row)

  -- Print discrete partitions for all divisors of N where 3 <= k < N (e.g., 3, 4, 6 for N=12)
  let divisors = [ k | k <- enumFromTo 3 (n - 1), n `mod` k == 0 ]
  mapM_ (\k -> do
           let segs = analyzeSegments (partitionRow k row)
           putStrLn $ "\n--- Discrete Partition (" ++ show (length segs) ++ " x " ++ show k ++ "-note segments) ---"
           mapM_ (printSegLine rootOffset) (zip (enumFromTo 1 (length segs)) segs))
        divisors

  -- Print sliding trichords (and tetrachords if N >= 5)
  let windowSizes = filter (< n) [3, 4]
  mapM_ (\k -> do
           let segs = analyzeSegments (slidingSegments k row)
           putStrLn $ "\n--- Sliding " ++ show k ++ "-Note Windows ---"
           mapM_ (printSegLine rootOffset) (zip (enumFromTo 1 (length segs)) segs))
        windowSizes

printSegLine :: Int -> (Int, SegmentInfo) -> IO ()
printSegLine rootOffset (idx, info) = do
  let notes = unwords [ pcToNoteName ((p + rootOffset) `mod` 12) | p <- siPitches info ]
      pcs   = "[" ++ intercalate "," (map show (siPitches info)) ++ "]"
      pf    = "(" ++ intercalate "," (map show (siPrimeForm info)) ++ ")"
      iv    = PCS.formatVector (siVector info)
  putStrLn $ "  #" ++ show idx ++ ": " ++ padRight 14 notes
          ++ " " ++ padRight 13 pcs
          ++ " -> Forte " ++ padRight 7 (siForteName info)
          ++ " " ++ padRight 11 pf
          ++ " " ++ iv
  where
    padRight len str = str ++ replicate (max 0 (len - length str)) ' '

--------------------------------------------------------------------------------
-- 4N Row-Invariance & Segment-Preservation Scanner
--------------------------------------------------------------------------------

data InvarianceMatch = InvarianceMatch
  { imLabel        :: String   -- ^ e.g. "P0", "I5", "RI11"
  , imNotes        :: [PitchClass]
  , imSameSet      :: Bool     -- ^ True if unordered pitch-class content equals P0
  , imFixedSlots   :: Int      -- ^ Count of ordinal positions i where form[i] == P0[i]
  , imPreservedK   :: [(Int, String)] -- ^ (Segment size k, "exact order" or "permuted")
  } deriving (Show, Eq)

-- | Generates all 4 * N row forms: (Label, Row) for P, R, I, and RI,
-- using the exact transposition labels from the N x N matrix headers.
allRowForms :: Int -> [PitchClass] -> [(String, [PitchClass])]
allRowForms m row =
  let mat = generateMatrix m row
      n   = length row
      rowHeaders = case mat of
                     (firstRow : _) -> map (\x -> (m - x) `mod` m) firstRow
                     _              -> enumFromTo 0 (n - 1)
      colHeaders = case mat of
                     (firstRow : _) -> firstRow
                     _              -> enumFromTo 0 (n - 1)
      idx = enumFromTo 0 (n - 1)
   in [ ("P"  ++ show (rowHeaders !! i), getPrime mat i)               | i <- idx ]
   ++ [ ("R"  ++ show (rowHeaders !! i), getRetrograde mat i)          | i <- idx ]
   ++ [ ("I"  ++ show (colHeaders !! i), getInversion mat i)           | i <- idx ]
   ++ [ ("RI" ++ show (colHeaders !! i), getRetrogradeInversion mat i) | i <- idx ]

-- | Scans all 4 * m row forms against P0 for:
-- 1. Unordered pitch-class set invariance (when N < m)
-- 2. Discrete k-note partition preservation (exact slot order or permuted)
-- 3. Fixed ordinal positions (for N >= 4, highlights forms locking >= 2 positions)
scanRowInvariance :: Int -> [PitchClass] -> [InvarianceMatch]
scanRowInvariance m row =
  let n = length row
      p0Set = PCS.makeSet row
      divisors = [ k | k <- enumFromTo 3 (n - 1), n `mod` k == 0 ]
      p0Parts = [ (k, map PCS.makeSet (partitionRow k row)) | k <- divisors ]
      forms = allRowForms m row
      evaluate (lbl, r) =
        let sameSet = n < m && PCS.makeSet r == p0Set
            fixed   = length [ () | (a, b) <- zip row r, a == b ]
            presK   = [ (k, mode)
                      | (k, origSegs) <- p0Parts
                      , let candSegs = map PCS.makeSet (partitionRow k r)
                      , Just mode <- [ if candSegs == origSegs
                                         then Just "exact slot order"
                                         else if sort candSegs == sort origSegs
                                                then Just "permuted slots"
                                                else Nothing
                                     ]
                      ]
            minFixed = if n >= 6 then 3 else 2
         in if lbl /= "P0" && (sameSet || not (null presK) || fixed >= minFixed)
              then Just InvarianceMatch
                     { imLabel      = lbl
                     , imNotes      = r
                     , imSameSet    = sameSet
                     , imFixedSlots = fixed
                     , imPreservedK = presK
                     }
              else Nothing
   in [ res | pair <- forms, Just res <- [evaluate pair] ]

-- | Prints a clean report of invariant row forms across all 4 * length row matrix transformations.
printRowInvariance :: Int -> Int -> [PitchClass] -> IO ()
printRowInvariance m rootOffset row = do
  let matches = scanRowInvariance m row
  putStrLn $ "\n=== Matrix Row-Invariance Scan (" ++ show (4 * length row) ++ " Forms vs. P0) ==="
  if null matches
    then putStrLn "No non-trivial segment, pitch-set, or positional invariances found."
    else mapM_ (printMatchLine m rootOffset) matches

printMatchLine :: Int -> Int -> InvarianceMatch -> IO ()
printMatchLine m rootOffset im = do
  let trans = map (\p -> (p + rootOffset) `mod` m) (imNotes im)
      noteStr = if m == 12
                  then unwords (map pcToNoteName trans)
                  else intercalate "," (map show trans)
      tags = [ "Same PC-set" | imSameSet im ]
          ++ [ "Preserves " ++ show k ++ "-note segments (" ++ mode ++ ")"
             | (k, mode) <- imPreservedK im ]
          ++ [ show (imFixedSlots im) ++ " fixed slot(s)" | imFixedSlots im > 0 ]
  putStrLn $ "  " ++ padRight 5 (imLabel im)
          ++ " [" ++ noteStr ++ "]"
  putStrLn $ "        -> " ++ intercalate " | " tags
  where
    padRight len str = str ++ replicate (max 0 (len - length str)) ' '

--------------------------------------------------------------------------------
-- Stravinsky / Krenek Rotational Arrays & Vertical Forte Analysis
--------------------------------------------------------------------------------

-- | Builds a Stravinsky rotational array from an N-note row:
-- Generates all N cyclic rotations, then transposes each rotation so its
-- initial pitch class matches the first pitch class of @row@.
rotationalArray :: Int -> [PitchClass] -> [[PitchClass]]
rotationalArray m row =
  case row of
    [] -> []
    (anchor : _) ->
      let n = length row
          rots = take n (iterate rotateLeft row)
          align rot =
            case rot of
              (firstPc : _) ->
                let shift = (anchor - firstPc) `mod` m
                 in map (\x -> (x + shift) `mod` m) rot
              [] -> []
       in map align rots
  where
    rotateLeft (x : xs) = xs ++ [x]
    rotateLeft []   = []

-- | Extracts the vertical columns (chords) of a rotational array, skipping Column 0
-- (which is always a unison of the anchor pitch).
rotationalVerticals :: Int -> [PitchClass] -> [[PitchClass]]
rotationalVerticals m row =
  case transpose (rotationalArray m row) of
    (_ : cols) -> cols
    []     -> []

-- | Prints the Stravinsky rotational array and analyzes every vertical column
-- using Allen Forte pitch-class set theory (when modulus == 12).
printRotationalArray :: Int -> Int -> [PitchClass] -> IO ()
printRotationalArray m rootOffset row = do
  let arr   = rotationalArray m row
      verts = rotationalVerticals m row
      shift p = (p + rootOffset) `mod` m
      fmtPitch p = if m == 12 then pcToNoteName (shift p) else show (shift p)
  putStrLn $ "\n=== Stravinsky Rotational Array (" ++ show (length row) ++ " x " ++ show (length row) ++ ") ==="
  mapM_ (\(idx, rot) -> do
           let cells = unwords [ padRight 3 (fmtPitch p) | p <- rot ]
           putStrLn $ "  Rot " ++ padRight 2 (show idx) ++ " | " ++ cells)
        (zip (enumFromTo 0 (length arr - 1)) arr)

  if m == 12
    then do
      putStrLn "\n--- Vertical Column Chords (Forte Set-Class Analysis) ---"
      mapM_ (\(colIdx, colPitches) -> do
               let uniqSet = PCS.makeSet colPitches
                   notes   = unwords [ fmtPitch p | p <- colPitches ]
                   pcsStr  = "[" ++ intercalate "," (map show uniqSet) ++ "]"
                   pf      = PCS.primeForm uniqSet
                   pfStr   = "(" ++ intercalate "," (map show pf) ++ ")"
                   ivStr   = PCS.formatVector (PCS.intervalVector pf)
               putStrLn $ "  Col " ++ padRight 2 (show colIdx)
                       ++ ": " ++ padRight 18 notes
                       ++ " -> " ++ padRight 15 pcsStr
                       ++ " Forte " ++ padRight 12 (PCS.forteName uniqSet)
                       ++ " " ++ padRight 13 pfStr
                       ++ " " ++ ivStr)
            (zip (enumFromTo 1 (length verts)) verts)
    else return ()
  where
    padRight len str = str ++ replicate (max 0 (len - length str)) ' '

--------------------------------------------------------------------------------
-- Derived & All-Interval 12-Tone Row Generators
--------------------------------------------------------------------------------

-- | Generates a 12-tone row derived from a 3-note trichord or 4-note tetrachord cell.
-- Every discrete partition of size @k@ (4 trichords or 3 tetrachords) is a Tn or TnI
-- transformation of @cell@, and together they cover all 12 pitch classes without repetition.
-- Returns @Nothing@ if @cell@ does not have 3 or 4 unique pitch classes, or if no valid
-- derived partition exists (e.g., symmetric cells like 3-12 [0,4,8] or 4-28 [0,3,6,9]).
deriveRow12 :: [PitchClass] -> StdGen -> (Maybe [PitchClass], StdGen)
deriveRow12 rawCell gen0 =
  let cell = PCS.makeSet rawCell
      k = length cell
   in if k /= 3 && k /= 4
        then (Nothing, gen0)
        else
          let numBlocks = 12 `div` k
              -- All Tn and TnI forms of the generator cell
              transforms = nub $
                [ PCS.transposeSet t cell | t <- enumFromTo 0 11 ]
                ++ [ PCS.invertSet t cell | t <- enumFromTo 0 11 ]
              -- Backtracking search for disjoint blocks covering 0..11
              search 0 _ _ = [[]]
              search step minStart used =
                [ blk : rest
                | blk <- transforms
                , case blk of
                    (firstPc : _) -> firstPc >= minStart
                    _             -> False
                , all (`notElem` used) blk
                , let nextMin = case blk of
                                  (firstPc : _) -> firstPc + 1
                                  _             -> minStart + 1
                , rest <- search (step - 1) nextMin (blk ++ used)
                ]
              solutions = search numBlocks 0 mempty

          in case solutions of
                [] -> (Nothing, gen0)
                _  ->
                  let (solIdx, gen1) = randomR (0, length solutions - 1) gen0
                      chosenBlocks = solutions !! solIdx
                      -- Shuffle block order and pitch order inside each block
                      (shuffledBlocks, gen2) = shuffleList chosenBlocks gen1
                      (orderedBlocks, gen3)  = mapAccumShuffle shuffledBlocks gen2
                   in (Just (concat orderedBlocks), gen3)
  where
    mapAccumShuffle [] g = ([], g)
    mapAccumShuffle (b : bs) g =
      let (sb, g1)  = shuffleList b g
          (sbs, g2) = mapAccumShuffle bs g1
       in (sb : sbs, g2)

-- | Generates a random All-Interval 12-tone row (where the 11 directed mod-12
-- intervals between consecutive pitches contain every interval 1..11 exactly once).
allIntervalRow12 :: StdGen -> ([PitchClass], StdGen)
allIntervalRow12 gen0 = go gen0
  where
    go g =
      let (startPc, g1) = randomR (0, 11) g
          (res, g2) = backtrack [startPc] [startPc] mempty g1
       in case res of
            Just row -> (row, g2)
            Nothing  -> go g2

    backtrack :: [PitchClass] -> [PitchClass] -> [Int] -> StdGen -> (Maybe [PitchClass], StdGen)
    backtrack revRow usedPcs usedInts g
      | length revRow == 12 = (Just (reverse revRow), g)
      | otherwise =
          let curr = case revRow of
                       (x : _) -> x
                       _       -> 0
              remSteps = 12 - length revRow
              -- In any all-interval 12-tone row, first and last pitch must differ by tritone (6)
              candidates =
                [ (nextPc, iv)
                | iv <- enumFromTo 1 11
                , iv `notElem` usedInts
                , let nextPc = (curr + iv) `mod` 12
                , nextPc `notElem` usedPcs
                , remSteps > 1 || ((nextPc - lastPc revRow) `mod` 12 == 6)
                ]
              (shuffledCands, g1) = shuffleList candidates g
           in tryCandidates shuffledCands g1
      where
        lastPc [x]      = x
        lastPc (_ : xs) = lastPc xs
        lastPc _        = 0

        tryCandidates [] gCur = (Nothing, gCur)
        tryCandidates ((nextPc, iv) : rest) gCur =
          let (attempt, gNext) = backtrack (nextPc : revRow) (nextPc : usedPcs) (iv : usedInts) gCur
           in case attempt of
                Just sol -> (Just sol, gNext)
                Nothing  -> tryCandidates rest gNext

-- | Pure Fisher-Yates style list shuffle using StdGen.
shuffleList :: [a] -> StdGen -> ([a], StdGen)
shuffleList [] g = ([], g)
shuffleList xs g =
  let (idx, g1) = randomR (0, length xs - 1) g
      (picked, rest) = extractAt idx xs
      (tailShuffled, g2) = shuffleList rest g1
   in (picked : tailShuffled, g2)
  where
    extractAt _ [] = error "extractAt: empty list"
    extractAt 0 (y : ys) = (y, ys)
    extractAt n (y : ys) =
      let (p, r) = extractAt (n - 1) ys
       in (p, y : r)
