module SerialComposer
  ( PitchClass
  , MidiPitch
  , Row
  , Matrix
  , NoteEvent(..)
  , PartSpec(..)
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
  ) where

import Prelude hiding (span)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Random (StdGen, randomR, randoms, split)
import Text.Printf (printf)

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
