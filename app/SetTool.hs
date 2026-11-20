module Main (main) where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toUpper)
import Data.List (elemIndex, intercalate)
import PitchClassSet
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Text.Read (readMaybe)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseNoteName :: String -> Maybe PitchClass
parseNoteName str =
  let norm = map toUpper (trim str)
      flats = ["C", "DB", "D", "EB", "E", "F", "GB", "G", "AB", "A", "BB", "B"]
      sharps = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in elemIndex norm flats <|> elemIndex norm sharps

parseToken :: String -> Maybe PitchClass
parseToken tok =
  let clean = trim tok
   in readMaybe clean <|> parseNoteName clean

splitCommas :: String -> [String]
splitCommas s =
  case break (== ',') s of
    (chunk, "")       -> [chunk]
    (chunk, _ : rest) -> chunk : splitCommas rest

-- | Parses either a Forte catalog name (e.g. "4-Z15") or a comma-separated list of pitches.
parseInput :: String -> Maybe PCSet
parseInput str =
  case lookupForte (trim str) of
    Just entry -> Just (fePrime entry)
    Nothing ->
      let parts = splitCommas str
       in if null parts || any (null . trim) parts
            then Nothing
            else makeSet <$> traverse parseToken parts

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [rawSet] ->
      case parseInput rawSet of
        Nothing -> do
          putStrLn $ "Error: Could not parse pitch-class set or Forte name '" ++ rawSet ++ "'."
          exitFailure
        Just pcs -> printAnalysis pcs

    ["supersets", rawSet] ->
      runCatalogQuery prog "Supersets" (findSupersets Nothing) rawSet

    ["supersets", rawSet, rawCard] ->
      case readMaybe rawCard of
        Just c -> runCatalogQuery prog ("Supersets (Cardinality " ++ show c ++ ")") (findSupersets (Just c)) rawSet
        Nothing -> do
          putStrLn $ "Error: Invalid cardinality '" ++ rawCard ++ "'."
          exitFailure

    ["subsets", rawSet] ->
      runCatalogQuery prog "Subsets" (findSubsets Nothing) rawSet

    ["subsets", rawSet, rawCard] ->
      case readMaybe rawCard of
        Just c -> runCatalogQuery prog ("Subsets (Cardinality " ++ show c ++ ")") (findSubsets (Just c)) rawSet
        Nothing -> do
          putStrLn $ "Error: Invalid cardinality '" ++ rawCard ++ "'."
          exitFailure

    [rawA, rawB] ->
      case (parseInput rawA, parseInput rawB) of
        (Just setA, Just setB) -> printComparison setA setB
        _ -> do
          putStrLn "Error: Could not parse one or both pitch-class sets."
          exitFailure

    _ -> do
      putStrLn "Usage:"
      putStrLn $ "  " ++ prog ++ " <set-or-forte-name>                 Analyze a single set"
      putStrLn $ "  " ++ prog ++ " <set-1> <set-2>                     Compare two sets (Z-relation & similarity)"
      putStrLn $ "  " ++ prog ++ " supersets <set> [cardinality]       Find catalog supersets containing <set>"
      putStrLn $ "  " ++ prog ++ " subsets <set> [cardinality]         Find catalog subsets contained in <set>"
      putStrLn "Examples:"
      putStrLn $ "  " ++ prog ++ " 4-Z15"
      putStrLn $ "  " ++ prog ++ " 4-Z15 4-Z29"
      putStrLn $ "  " ++ prog ++ " supersets 3-5 5"
      putStrLn $ "  " ++ prog ++ " subsets 6-Z19 4"
      exitFailure

runCatalogQuery :: String -> String -> (PCSet -> [(ForteEntry, Int)]) -> String -> IO ()
runCatalogQuery _ label searchFn rawSet =
  case parseInput rawSet of
    Nothing -> do
      putStrLn $ "Error: Could not parse pitch-class set or Forte name '" ++ rawSet ++ "'."
      exitFailure
    Just pcs -> do
      let pf = primeForm pcs
          results = searchFn pf
      printf "=== %s of %s (%s) ===\n"
        label (forteName pf) (intercalate "," (map show pf))
      if null results
        then putStrLn "No matching set classes found in Forte catalog."
        else do
          printf "Found %d set class(es):\n\n" (length results)
          mapM_ (\(entry, count) ->
                   printf "  %-7s (%-17s) %s  [x%d]\n"
                     (feName entry)
                     (intercalate "," (map show (fePrime entry)))
                     (formatVector (feVector entry))
                     count)
                results

printAnalysis :: PCSet -> IO ()
printAnalysis pcs = do
  let no   = normalOrder pcs
      pf   = primeForm pcs
      fNum = forteName pcs
      iv   = intervalVector pcs
      comp = complementSet pcs
      ct   = commonTones pcs

  putStrLn "=== Pitch-Class Set Analysis ==="
  putStrLn $ "Forte Name:         " ++ fNum
  putStrLn $ "Input Set (sorted): " ++ formatSet pcs ++ " (Cardinality: " ++ show (length pcs) ++ ")"
  putStrLn $ "Normal Order:       " ++ formatSet no
  putStrLn $ "Prime Form:         (" ++ intercalate "," (map show pf) ++ ")"
  putStrLn $ "Interval Vector:    " ++ formatVector iv
  putStrLn $ "Complement Set:     " ++ formatSet comp
          ++ " -> Forte " ++ forteName comp
          ++ " (" ++ intercalate "," (map show (primeForm comp)) ++ ")"

  case findZMate pcs of
    Just zMate ->
      putStrLn $ "Z-Related Mate:     " ++ feName zMate
              ++ " (" ++ intercalate "," (map show (fePrime zMate)) ++ ")"
              ++ " " ++ formatVector (feVector zMate)
    Nothing ->
      putStrLn "Z-Related Mate:     None"

  putStrLn "\n--- Transpositional Common Tones (Tn) ---"
  putStrLn $ unwords [ printf "T%d:%d" t count | (t, count) <- ct ]

  let simSets = findSimilarSets pcs
  if not (null simSets)
    then do
      putStrLn "\n--- Similar Set Classes (Forte R1 / R2 / Rp) ---"
      mapM_ (\(entry, rels) ->
               printf "  %-7s (%-15s) %s  [%s]\n"
                 (feName entry)
                 (intercalate "," (map show (fePrime entry)))
                 (formatVector (feVector entry))
                 (intercalate ", " rels))
            simSets
    else return ()

  if length pcs > 3 && length pcs <= 7
    then do
      putStrLn "\n--- Embedded Trichord Subsets ---"
      mapM_ (\(sub, spf) ->
               printf "  %-10s -> %-6s (%s)\n"
                 (formatSet sub)
                 (forteName spf)
                 (intercalate "," (map show spf)))
            (subsetsOfSize 3 pcs)
    else return ()

printComparison :: PCSet -> PCSet -> IO ()
printComparison setA setB = do
  let pfA  = primeForm setA
      pfB  = primeForm setB
      ivA  = intervalVector pfA
      ivB  = intervalVector pfB
      zRel = isZRelated setA setB
      rels = similarityRelation setA setB

  putStrLn "=== Pitch-Class Set Comparison ==="
  printf "Set A: %-14s -> Forte %-7s (%s)  %s\n"
    (formatSet setA) (forteName setA) (intercalate "," (map show pfA)) (formatVector ivA)
  printf "Set B: %-14s -> Forte %-7s (%s)  %s\n"
    (formatSet setB) (forteName setB) (intercalate "," (map show pfB)) (formatVector ivB)
  putStrLn ""
  putStrLn $ "Same Set Class (Tn/TnI): " ++ if pfA == pfB then "Yes" else "No"
  putStrLn $ "Z-Related Pair:          " ++ if zRel then "Yes (identical interval vector, distinct prime forms)" else "No"
  putStrLn $ "Similarity Relations:    " ++ if null rels then "None" else intercalate ", " rels
