module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isSpace, toUpper)
import Data.List (elemIndex)
import SerialComposer
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Random (StdGen, getStdGen, mkStdGen)
import Text.Read (readMaybe)

data Options = Options
  { optCustomRow   :: Maybe String
  , optDeriveCell  :: Maybe String
  , optAllInterval :: Bool
  , optLength      :: Int
  , optMod         :: Int
  , optSeed        :: Maybe Int
  , optRoot        :: String
  , optOctave      :: Maybe Int
  , optMode        :: String -- "guitar", "bass", or "duo"
  , optAnalyze     :: Bool
  , optM5          :: Bool
  , optM7          :: Bool
  , optRotate      :: Bool
  , optInvariance  :: Bool
  , optPlain       :: Bool
  , optBpm         :: Int
  , optOut         :: String
  , optHelp        :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optCustomRow   = Nothing
  , optDeriveCell  = Nothing
  , optAllInterval = False
  , optLength      = 6
  , optMod         = 12
  , optSeed        = Nothing
  , optRoot        = "C"
  , optOctave      = Nothing
  , optMode        = "duo"
  , optAnalyze     = False
  , optM5          = False
  , optM7          = False
  , optRotate      = False
  , optInvariance  = False
  , optPlain       = False
  , optBpm         = 75
  , optOut         = "serial_output"
  , optHelp        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['w'] ["row"]
      (ReqArg (\v opts -> opts { optCustomRow = Just v }) "LIST")
      "Custom comma-separated notes or integers, e.g. C,Db,Gb,G,Bb or 0,1,6,7,10"
  , Option ['d'] ["derive"]
      (ReqArg (\v opts -> opts { optDeriveCell = Just v }) "CELL")
      "Generate a 12-tone derived row from a 3- or 4-note cell (e.g. 0,1,6 or C,Db,Gb)"
  , Option ['i'] ["all-interval"]
      (NoArg (\opts -> opts { optAllInterval = True }))
      "Generate a 12-tone all-interval row (contains every interval 1..11 once)"
  , Option ['n'] ["length"]
      (ReqArg (\v opts -> opts { optLength = read v }) "INT")
      "Number of unique pitches in random row (default: 6)"
  , Option ['m'] ["mod"]
      (ReqArg (\v opts -> opts { optMod = read v }) "INT")
      "Modulus / chromatic division (default: 12)"
  , Option ['s'] ["seed"]
      (ReqArg (\v opts -> opts { optSeed = Just (read v) }) "INT")
      "Deterministic integer RNG seed (default: random system seed)"
  , Option ['r'] ["root"]
      (ReqArg (\v opts -> opts { optRoot = v }) "NOTE")
      "Root note name: C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B (default: C)"
  , Option ['o'] ["octave"]
      (ReqArg (\v opts -> opts { optOctave = Just (read v) }) "INT")
      "Override base octave for guitar (default: auto based on root)"
  , Option ['p'] ["part"]
      (ReqArg (\v opts -> opts { optMode = v }) "MODE")
      "Instrument mode: guitar, bass, or duo (default: duo)"
  , Option ['a'] ["analyze"]
      (NoArg (\opts -> opts { optAnalyze = True }))
      "Print Forte pitch-class set segmentation of P0 (requires --mod 12)"
  , Option [] ["m5"]
      (NoArg (\opts -> opts { optM5 = True }))
      "Apply M5 cycle-of-fourths transform (x -> 5x mod m) to the row"
  , Option [] ["m7"]
      (NoArg (\opts -> opts { optM7 = True }))
      "Apply M7 cycle-of-fifths transform (x -> 7x mod m) to the row"
  , Option ['R'] ["rotate"]
      (NoArg (\opts -> opts { optRotate = True }))
      "Print Stravinsky rotational array and vertical column Forte analysis"
  , Option ['v'] ["invariance"]
      (NoArg (\opts -> opts { optInvariance = True }))
      "Scan all 4N matrix forms for segment, pitch-set, and positional invariance"
  , Option [] ["plain"]
      (NoArg (\opts -> opts { optPlain = True }))
      "Use plain quarter notes without rhythm/octave displacement"
  , Option ['t'] ["tempo"]
      (ReqArg (\v opts -> opts { optBpm = read v }) "BPM")
      "Tempo in BPM for LilyPond MIDI export (default: 75)"
  , Option ['f'] ["out"]
      (ReqArg (\v opts -> opts { optOut = v }) "PREFIX")
      "Output filename prefix without extension (default: serial_output)"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help message"
  ]

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | Parses a single note name (C..B, flats or sharps, case-insensitive) to 0..11.
parseNoteName :: String -> Maybe PitchClass
parseNoteName str =
  let norm = map toUpper (trim str)
      flats = ["C", "DB", "D", "EB", "E", "F", "GB", "G", "AB", "A", "BB", "B"]
      sharps = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in elemIndex norm flats <|> elemIndex norm sharps

-- | Parses a single token as either an integer pitch class or a note name.
parsePitchToken :: String -> Maybe PitchClass
parsePitchToken tok =
  let clean = trim tok
   in readMaybe clean <|> parseNoteName clean

-- | Parses a comma-separated list of integers and/or note names into a Row.
parseCustomRow :: String -> Maybe Row
parseCustomRow str =
  let parts = splitCommas str
   in if null parts || any (null . trim) parts
        then Nothing
        else traverse parsePitchToken parts
  where
    splitCommas :: String -> [String]
    splitCommas s =
      case break (== ',') s of
        (chunk, [])       -> [chunk]
        (chunk, _ : rest) -> chunk : splitCommas rest

defaultGuitarOctave :: PitchClass -> Int
defaultGuitarOctave pc
  | pc >= 9   = 1 -- A1, Bb1, B1
  | otherwise = 2 -- C2 .. Ab2

buildParts :: Options -> PitchClass -> Matrix -> StdGen -> [PartSpec]
buildParts opts rootPc mat gen =
  let n = length mat
      idx2 = min 2 (n - 1)
      idx3 = min 3 (n - 1)
      gtrOct = maybe (defaultGuitarOctave rootPc) id (optOctave opts)
      bassOct = max 0 (gtrOct - 1)

      gtrRows =
        [ ("Section A (P0)", getPrime mat 0)
        , ("Section B (I0)", getInversion mat 0)
        ]
      bassRows =
        [ ("Section A (R" ++ show idx2 ++ ")", getRetrograde mat idx2)
        , ("Section B (RI" ++ show idx3 ++ ")", getRetrogradeInversion mat idx3)
        ]

      (gtrSecs, g1) =
        if optPlain opts
          then ([(lbl, realizePlain rootPc gtrOct r) | (lbl, r) <- gtrRows], gen)
          else realizeSectionsGroove rootPc gtrOct 2 25 gen gtrRows

      (bassSecs, _) =
        if optPlain opts
          then ([(lbl, realizePlain rootPc bassOct r) | (lbl, r) <- bassRows], g1)
          else realizeSectionsGroove rootPc bassOct 1 15 g1 bassRows

      guitarPart = PartSpec "Distorted Guitar" "guitar" "distorted guitar" gtrSecs
      bassPart = PartSpec "Distorted Bass" "bass" "electric bass (pick)" bassSecs
   in case optMode opts of
        "guitar" -> [guitarPart]
        "bass"   -> [bassPart]
        _        -> [guitarPart, bassPart]

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTIONS...]"

  opts <- case getOpt Permute options args of
    (o, _, []) -> return (foldl (flip id) defaultOptions o)
    (_, _, errs) -> do
      putStrLn (concat errs ++ usageInfo header options)
      exitFailure

  let m = optMod opts

  if optHelp opts
    then do
      putStrLn (usageInfo header options)
      exitSuccess
    else return ()

  rootPc <- case parseNoteName (optRoot opts) of
    Just pc -> return pc
    Nothing -> do
      putStrLn $ "Error: Unknown root note '" ++ optRoot opts ++ "'."
      exitFailure

  sysGen <- getStdGen
  let initGen = maybe sysGen mkStdGen (optSeed opts)

  (rawRow, genAfterRow) <-
    if optAllInterval opts
    then do
      when (m /= 12) $ do
        putStrLn "Error: --all-interval requires --mod 12."
        exitFailure
      return (allIntervalRow12 initGen)
    else case optDeriveCell opts of
      Just cellStr -> do
        when (m /= 12) $ do
          putStrLn "Error: --derive requires --mod 12."
          exitFailure
        case parseCustomRow cellStr of
          Nothing -> do
            putStrLn $ "Error: Invalid generator cell '" ++ cellStr ++ "'."
            exitFailure
          Just cell ->
            case deriveRow12 cell initGen of
              (Nothing, _) -> do
                putStrLn $ "Error: Cannot derive a 12-tone row from " ++ show cell
                        ++ " (requires 3 or 4 non-symmetric pitch classes)."
                exitFailure
              (Just derived, gNext) -> return (derived, gNext)
      Nothing ->
        case optCustomRow opts of
          Just rStr ->
            case parseCustomRow rStr of
              Just parsed -> return (parsed, initGen)
              Nothing -> do
                putStrLn $ "Error: Could not parse custom row '" ++ rStr ++ "'."
                exitFailure
          Nothing ->
            return (randomRow m (optLength opts) initGen)

  let row
        | optM5 opts = m5Row m rawRow
        | optM7 opts = m7Row m rawRow
        | otherwise  = rawRow

  let mat = generateMatrix (optMod opts) row

  putStrLn $ "=== Matrix (" ++ show (length row) ++ "-Tone Row, mod " ++ show (optMod opts) ++ ") ==="
  putStrLn $ "Input Row:     " ++ show row
  putStrLn $ "Root Tuning:   " ++ optRoot opts ++ "\n"

  putStrLn "--- Integer Matrix ---"
  printMatrix mat
  putStrLn "\n--- Transposed Note Matrix ---"
  printNoteMatrix rootPc mat

  if optAnalyze opts
    then if optMod opts == 12
           then printRowSegmentation rootPc row
           else putStrLn "\nNote: --analyze requires --mod 12 (standard chromatic pitch-class space)."
    else return ()

  if optInvariance opts
    then printRowInvariance m rootPc row
    else return ()

  if optRotate opts
    then printRotationalArray m rootPc row
    else return ()

  let parts = buildParts opts rootPc mat genAfterRow
      xmlFile = optOut opts ++ ".musicxml"
      lyFile = optOut opts ++ ".ly"

  exportMusicXMLParts xmlFile parts
  exportLilyPondParts lyFile (optBpm opts) parts

  putStrLn "\nExported:"
  putStrLn $ "  -> " ++ xmlFile
  putStrLn $ "  -> " ++ lyFile
