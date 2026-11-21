module Main (main) where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toUpper)
import Data.List (elemIndex, intercalate)
import PitchClassSet
import Polytonal
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

    ["complex", rawSet] ->
      case parseInput rawSet of
        Nothing -> do
          putStrLn $ "Error: Could not parse pitch-class set or Forte name '" ++ rawSet ++ "'."
          exitFailure
        Just pcs -> printKComplex pcs

    ("nexus" : rawSets) | length rawSets >= 2 ->
      case traverse parseInput rawSets of
        Nothing -> do
          putStrLn "Error: Could not parse one or more pitch-class sets."
          exitFailure
        Just pcSets -> printNexusSets pcSets

    ["comb", rawSet] ->
      case parseInput rawSet of
        Nothing -> do
          putStrLn $ "Error: Could not parse pitch-class set or Forte name '" ++ rawSet ++ "'."
          exitFailure
        Just pcs -> printCombinatoriality pcs

    ["voicelead", rawSrc, rawTgt] ->
      case (parseInput rawSrc, parseInput rawTgt) of
        (Just src, Just tgt) -> printVoiceLeading src tgt
        _ -> do
          putStrLn "Error: Could not parse source or target pitch-class set."
          exitFailure

    ["cycle", rawSpec] ->
      printCycleSpec rawSpec 0

    ["cycle", rawSpec, rawStart] ->
      case parseToken rawStart of
        Just st -> printCycleSpec rawSpec st
        Nothing -> do
          putStrLn $ "Error: Could not parse starting pitch '" ++ rawStart ++ "'."
          exitFailure

    ["cycle-fit", rawSet] ->
      case parseInput rawSet of
        Nothing -> do
          putStrLn $ "Error: Could not parse pitch-class set or Forte name '" ++ rawSet ++ "'."
          exitFailure
        Just pcs -> printCycleFits pcs

    ["poly", rawA, rawB] ->
      case (parseLayerSpec rawA, parseLayerSpec rawB) of
        (Just layA, Just layB) -> printPolyAnalysis (analyzeBitonal layA layB)
        _ -> do
          putStrLn "Error: Could not parse one or both tonal layers (e.g. B:phrygian or 11,2,6)."
          exitFailure

    ["poly-step", rawA1, rawB1, rawA2, rawB2] ->
      case (parseLayerSpec rawA1, parseLayerSpec rawB1, parseLayerSpec rawA2, parseLayerSpec rawB2) of
        (Just a1, Just b1, Just a2, Just b2) ->
          printPolyStepReport (evaluatePolyStep (a1, b1) (a2, b2))
        _ -> do
          putStrLn "Error: Could not parse one or more layer specs (e.g. B:maj F:maj Db:min G:min)."
          exitFailure

    ["poly-partners", rawA] ->
      case parseLayerSpec rawA of
        Just layA -> printBitonalPartners layA Nothing
        Nothing -> do
          putStrLn $ "Error: Could not parse tonal layer '" ++ rawA ++ "'."
          exitFailure

    ["poly-partners", rawA, rawMode] ->
      case (parseLayerSpec rawA, parseModeName rawMode) of
        (Just layA, Just (_, mode)) -> printBitonalPartners layA (Just mode)
        (Nothing, _) -> do
          putStrLn $ "Error: Could not parse tonal layer '" ++ rawA ++ "'."
          exitFailure
        (_, Nothing) -> do
          putStrLn $ "Error: Unknown mode or chord type '" ++ rawMode ++ "'."
          exitFailure

    ("poly-multi" : rawLayers) | length rawLayers >= 3 ->
      case traverse parseLayerSpec rawLayers of
        Nothing -> do
          putStrLn "Error: Could not parse one or more tonal layers (e.g. B:min F:min D:maj)."
          exitFailure
        Just layers ->
          case analyzeMultiPoly layers of
            Nothing -> exitFailure
            Just mpa -> printMultiPolyAnalysis mpa

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
      putStrLn $ "  " ++ prog ++ " complex <set>                     Show Forte K / Kh set-complex around <set>"
      putStrLn $ "  " ++ prog ++ " nexus <set1> <set2> [set3...]     Find nexus sets in K/Kh with all given sets"
      putStrLn $ "  " ++ prog ++ " comb <hexachord>                Analyze 6-note hexachord combinatoriality"
      putStrLn $ "  " ++ prog ++ " voicelead <src> <target-class>    Find most parsimonious Tn/TnI voice-leadings"
      putStrLn $ "  " ++ prog ++ " cycle <k | k1,k2> [start]       Generate basic Ck or alternating Alt(k1,k2) cycle"
      putStrLn $ "  " ++ prog ++ " cycle-fit <set>                 Find interval cycles containing <set>"
      putStrLn $ "  " ++ prog ++ " poly <layerA> <layerB>            Analyze bitonal friction & resolution traps"
      putStrLn $ "  " ++ prog ++ " poly-step <A1> <B1> <A2> <B2>     Validate transition against accidental resolutions"
      putStrLn $ "  " ++ prog ++ " poly-partners <layerA> [mode]     Rank highest-ambiguity bitonal partners"
      putStrLn $ "  " ++ prog ++ " poly-multi <L1> <L2> <L3> [...]   Analyze 3+ simultaneous tonal layers"
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
      rNo  = rahnNormalOrder pcs
      rPf  = rahnPrimeForm pcs
      fNum = forteName pcs
      iv   = intervalVector pcs
      comp = complementSet pcs
      ct   = commonTones pcs

  putStrLn "=== Pitch-Class Set Analysis ==="
  putStrLn $ "Forte Name:         " ++ fNum
  putStrLn $ "Input Set (sorted): " ++ formatSet pcs ++ " (Cardinality: " ++ show (length pcs) ++ ")"
  putStrLn $ "Normal Order:       " ++ formatSet no
          ++ if rNo /= no then "  (Rahn: " ++ formatSet rNo ++ ")" else ""
  putStrLn $ "Prime Form:         (" ++ intercalate "," (map show pf) ++ ")"
          ++ if rPf /= pf
               then "  (Rahn: (" ++ intercalate "," (map show rPf) ++ "))"
               else "  (Forte & Rahn identical)"
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

  let m5 = m5Set pcs
      m7 = m7Set pcs
  putStrLn $ "M5 Transform (x5):  " ++ formatSet m5
          ++ " -> Forte " ++ forteName m5
          ++ " (" ++ intercalate "," (map show (primeForm m5)) ++ ") "
          ++ formatVector (intervalVector m5)
  putStrLn $ "M7 Transform (x7):  " ++ formatSet m7
          ++ " -> Forte " ++ forteName m7
          ++ " (" ++ intercalate "," (map show (primeForm m7)) ++ ") "
          ++ formatVector (intervalVector m7)

  putStrLn "\n--- Transpositional Common Tones (Tn) ---"
  putStrLn $ unwords [ printf "T%d:%d" t count | (t, count) <- ct ]

  let ict = inversionCommonTones pcs
      (symT, symI) = solveTransformations pcs pcs
  putStrLn "\n--- Inversional Common Tones (TnI) ---"
  putStrLn $ unwords [ printf "T%dI:%d" t count | (t, count) <- ict ]
  putStrLn $ "Internal Symmetry:  "
          ++ intercalate ", " ([ "T" ++ show n | n <- symT ] ++ [ "T" ++ show n ++ "I" | n <- symI ])

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
      (tOps, iOps) = solveTransformations setA setB
      opsList = [ "T" ++ show n | n <- tOps ] ++ [ "T" ++ show n ++ "I" | n <- iOps ]

  putStrLn "=== Pitch-Class Set Comparison ==="
  printf "Set A: %-14s -> Forte %-7s (%s)  %s\n"
    (formatSet setA) (forteName setA) (intercalate "," (map show pfA)) (formatVector ivA)
  printf "Set B: %-14s -> Forte %-7s (%s)  %s\n"
    (formatSet setB) (forteName setB) (intercalate "," (map show pfB)) (formatVector ivB)
  putStrLn ""
  putStrLn $ "Same Set Class (Tn/TnI): " ++ if pfA == pfB then "Yes" else "No"
  putStrLn $ "Exact Mapping (A -> B):  " ++ if null opsList then "None (different set classes)" else intercalate ", " opsList
  putStrLn $ "Z-Related Pair:          " ++ if zRel then "Yes (identical interval vector, distinct prime forms)" else "No"
  putStrLn $ "Similarity Relations:    " ++ if null rels then "None" else intercalate ", " rels

printKComplex :: PCSet -> IO ()
printKComplex pcs = do
  let pf   = primeForm pcs
      pfC  = primeForm (complementSet pcs)
      members = findKComplex pf
      khCount = length [ () | (_, tag) <- members, tag == "Kh" ]
  printf "=== Forte Set-Complex K / Kh for %s (%s) & Complement %s (%s) ===\n"
    (forteName pf) (intercalate "," (map show pf))
    (forteName pfC) (intercalate "," (map show pfC))
  printf "Total K members: %d (including %d Kh members)\n\n" (length members) khCount
  mapM_ (\(entry, tag) ->
           printf "  [%-2s]  %-7s (%-17s) %s\n"
             tag
             (feName entry)
             (intercalate "," (map show (fePrime entry)))
             (formatVector (feVector entry)))
        members

printNexusSets :: [PCSet] -> IO ()
printNexusSets pcSets = do
  let labels = [ forteName s ++ " (" ++ intercalate "," (map show (primeForm s)) ++ ")" | s <- pcSets ]
      nexus  = findNexusSets False pcSets
      khOnly = [ (e, t) | (e, t) <- nexus, t == "Kh" ]
  putStrLn "=== Forte Nexus Set Search ==="
  putStrLn $ "Target Sets: " ++ intercalate ", " labels
  printf "Found %d K-nexus set class(es) (%d strict Kh-nexus):\n\n" (length nexus) (length khOnly)
  mapM_ (\(entry, tag) ->
           printf "  [%-2s]  %-7s (%-17s) %s\n"
             tag
             (feName entry)
             (intercalate "," (map show (fePrime entry)))
             (formatVector (feVector entry)))
        nexus

printCombinatoriality :: PCSet -> IO ()
printCombinatoriality pcs =
  case analyzeCombinatoriality pcs of
    Nothing -> do
      putStrLn $ "Error: Combinatoriality requires a hexachord (cardinality 6), got cardinality "
              ++ show (length pcs) ++ "."
      exitFailure
    Just res -> do
      let h    = crHexachord res
          comp = crComplement res
          pStr = if null (crPTransLevels res)
                   then "None"
                   else intercalate ", " [ "T" ++ show n | n <- crPTransLevels res ]
          iStr = if null (crITransLevels res)
                   then "None"
                   else intercalate ", " [ "T" ++ show n ++ "I" | n <- crITransLevels res ]
      printf "=== Hexachordal Combinatoriality Analysis ===\n"
      printf "Hexachord H:      %s -> Forte %-7s (%s)  %s\n"
        (formatSet h) (forteName h) (intercalate "," (map show (primeForm h))) (formatVector (intervalVector h))
      printf "Complement H-bar: %s -> Forte %-7s (%s)  %s\n"
        (formatSet comp) (forteName comp) (intercalate "," (map show (primeForm comp))) (formatVector (intervalVector comp))
      putStrLn ""
      putStrLn $ "Classification:   " ++ crClassification res
      putStrLn $ "P / R Levels:     " ++ pStr
      putStrLn $ "I / RI Levels:    " ++ iStr

printVoiceLeading :: PCSet -> PCSet -> IO ()
printVoiceLeading src tgt =
  case minimalVoiceLeading src tgt of
    Nothing -> do
      putStrLn $ "Error: Minimal voice-leading requires two non-empty sets of equal cardinality (got "
              ++ show (length src) ++ " and " ++ show (length tgt) ++ ")."
      exitFailure
    Just moves -> do
      let topMoves = take 6 moves
      printf "=== Minimal Voice-Leading Map: %s (%s) -> Target Class %s (%s) ===\n\n"
        (formatSet src) (forteName src)
        (forteName tgt) (intercalate "," (map show (primeForm tgt)))
      mapM_ (\m -> do
               let moveStrs = [ show a ++ "->" ++ show b ++ "(" ++ fmtDelta d ++ ")"
                              | (a, b, d) <- vlPairing m ]
               printf "  [Dist: %2d]  %-14s -> %-14s  |  %s\n"
                 (vlDistance m)
                 (vlOpLabel m)
                 (formatSet (vlTarget m))
                 (unwords moveStrs))
            topMoves
  where
    fmtDelta d
      | d > 0     = "+" ++ show d
      | otherwise = show d


printCycleSpec :: String -> PitchClass -> IO ()
printCycleSpec rawSpec startPc =
  case splitCommas (trim rawSpec) of
    [single] ->
      case readMaybe (trim single) of
        Just k | k >= 1 && k <= 11 -> do
          let cosets = cycleCosets k startPc
              primary = case cosets of
                          (c : _) -> c
                          []      -> []
          printf "=== Interval Cycle C%d (Start: %d) ===\n" k (startPc `mod` 12)
          printf "Primary Orbit:  %s -> Forte %-10s (%s)  %s\n"
            (formatSet primary)
            (forteName primary)
            (intercalate "," (map show (primeForm primary)))
            (formatVector (intervalVector primary))
          printf "Total Cosets:   %d (partitioning all 12 pitch classes)\n\n" (length cosets)
          mapM_ (\(idx, orb) ->
                   printf "  Coset %d: %s\n" idx (formatSet orb))
                (zip (enumFromTo 1 (length cosets)) cosets)
        _ -> do
          putStrLn "Error: Interval step must be an integer between 1 and 11 (or a pair like 1,3)."
          exitFailure

    [s1, s2] ->
      case (readMaybe (trim s1), readMaybe (trim s2)) of
        (Just k1, Just k2) | k1 >= 1 && k1 <= 11 && k2 >= 1 && k2 <= 11 -> do
          let orb = alternatingCycle (k1, k2) startPc
              uniq = makeSet orb
          printf "=== Alternating Interval Cycle Alt(%d,%d) (Start: %d) ===\n" k1 k2 (startPc `mod` 12)
          printf "Ordered Cycle:  %s (Length: %d)\n" (formatSet orb) (length orb)
          printf "Unique PC-Set:  %s -> Forte %-10s (%s)  %s\n"
            (formatSet uniq)
            (forteName uniq)
            (intercalate "," (map show (primeForm uniq)))
            (formatVector (intervalVector uniq))
        _ -> do
          putStrLn "Error: Alternating interval steps must be integers between 1 and 11 (e.g. 1,3)."
          exitFailure

    _ -> do
      putStrLn "Error: Expected a single interval (e.g. 3) or pair of intervals (e.g. 1,3)."
      exitFailure

printCycleFits :: PCSet -> IO ()
printCycleFits pcs = do
  let fits = findCycleFits pcs
  printf "=== Interval-Cycle Alignment for %s (%s) ===\n"
    (formatSet pcs) (forteName pcs)
  if null fits
    then putStrLn "No non-trivial symmetric interval cycles contain this set."
    else do
      printf "Found %d matching cyclic collection(s):\n\n" (length fits)
      mapM_ (\cf ->
               printf "  %-22s -> %-18s  Forte %-8s  %s\n"
                 (cfLabel cf)
                 (formatSet (cfOrbit cf))
                 (cfForteName cf)
                 (if cfIsContig cf then "[CONTIGUOUS]" else "[SUBSET]"))
            fits

printPolyAnalysis :: PolyAnalysis -> IO ()
printPolyAnalysis pa = do
  let layA = paLayerA pa
      layB = paLayerB pa
      fmtPCs xs = if null xs then "None" else intercalate ", " (map pcName xs)
      fmtPairs ps = if null ps
                      then "None"
                      else intercalate ", " [ pcName a ++ "-" ++ pcName b | (a, b) <- ps ]
  putStrLn "=== Polytonal / Bitonal Ambiguity Analysis ==="
  printf "Layer A:           %-22s %s\n" (plLabel layA) (formatSet (plPitches layA))
  printf "Layer B:           %-22s %s\n" (plLabel layB) (formatSet (plPitches layB))
  printf "Combined Union:    %s -> Forte %-8s (%s)  %s\n"
    (formatSet (paUnion pa))
    (paUnionForte pa)
    (intercalate "," (map show (paUnionPrime pa)))
    (formatVector (paUnionVector pa))
  printf "Ambiguity Score:   %d / 100\n" (paAmbiguityScore pa)
  putStrLn $ "Assessment:        " ++ paVerdict pa

  putStrLn "\n--- Bitonal Friction vs. Absorption ---"
  putStrLn $ "Shared Pivot PCs:  " ++ fmtPCs (paSharedPitches pa)
  putStrLn $ "Cross Half-Steps:  " ++ fmtPairs (paCrossHalfSteps pa)
  putStrLn $ "Cross Tritones:    " ++ fmtPairs (paCrossTritones pa)

  putStrLn "\n--- Anti-Resolution & Scale-Collapse Warnings ---"
  if null (paParentScaleHits pa)
    then putStrLn "Parent Scale Fit:  None (avoids single-scale diatonic/minor absorption)"
    else putStrLn $ "Parent Scale Fit:  WARNING — fits inside "
                 ++ intercalate ", " (paParentScaleHits pa)

  if null (paDominantTraps pa)
    then putStrLn "Accidental V7s:    None detected across layers"
    else do
      putStrLn "Accidental V7s:    WARNING — cross-layer dominant 7th chords detected:"
      mapM_ (\dt -> do
               let (thirdPc, seventhPc) = dtTritone dt
                   res3 = (thirdPc + 1) `mod` 12
                   res7 = (seventhPc - 1) `mod` 12
               printf "  * %s7 (%s) pulls toward Tonic %s (avoid resolving tritone %s-%s -> %s-%s)\n"
                 (pcName (dtDomRoot dt))
                 (intercalate "," (map pcName (dtDomPitches dt)))
                 (pcName (dtTargetTonic dt))
                 (pcName thirdPc) (pcName seventhPc)
                 (pcName res3) (pcName res7))
            (paDominantTraps pa)


printPolyStepReport :: PolyStepReport -> IO ()
printPolyStepReport rep = do
  let s1 = psState1 rep
      s2 = psState2 rep
      heldStr = if null (psCommonTones rep)
                  then "None"
                  else intercalate ", " (map pcName (psCommonTones rep))
  putStrLn "=== Polytonal Progression Step Validator ==="
  printf "State 1:        %s + %s  (Union: %s, Ambiguity: %d/100)\n"
    (plLabel (paLayerA s1)) (plLabel (paLayerB s1))
    (paUnionForte s1) (paAmbiguityScore s1)
  printf "State 2:        %s + %s  (Union: %s, Ambiguity: %d/100)\n"
    (plLabel (paLayerA s2)) (plLabel (paLayerB s2))
    (paUnionForte s2) (paAmbiguityScore s2)
  putStrLn $ "Held Pivots:    " ++ heldStr
  putStrLn $ "Ambiguity Safe: " ++ if psPreservesAmb rep
                                     then "YES — avoids resolution traps and sustains bitonality"
                                     else "NO — resolution or tonal absorption hazard(s) detected"

  if null (psHazards rep)
    then putStrLn "\nNo anti-resolution hazards detected."
    else do
      putStrLn "\n--- Resolution & Absorption Hazards to Avoid ---"
      mapM_ (\h -> printf "  [!] %-24s: %s\n" (shCategory h) (shDescription h))
            (psHazards rep)

printBitonalPartners :: PolyLayer -> Maybe ScaleMode -> IO ()
printBitonalPartners layA mMode = do
  let ranked = take 12 (findBitonalPartners layA mMode)
  printf "=== Top Non-Resolving Bitonal Partners for %s %s ===\n\n"
    (plLabel layA) (formatSet (plPitches layA))
  mapM_ (\pa -> do
           let layB = paLayerB pa
               trapFlag = if null (paDominantTraps pa)
                            then "0 V7 traps"
                            else show (length (paDominantTraps pa)) ++ " V7 trap(s)"
               scaleFlag = if null (paParentScaleHits pa)
                             then "No scale collapse"
                             else "Collapses to 1 scale"
           printf "  [Score: %3d]  %-20s -> Union %-8s | ic1:%-2d ic6:%-2d | %-17s | %s\n"
             (paAmbiguityScore pa)
             (plLabel layB)
             (paUnionForte pa)
             (length (paCrossHalfSteps pa))
             (length (paCrossTritones pa))
             scaleFlag
             trapFlag)
        ranked

printMultiPolyAnalysis :: MultiPolyAnalysis -> IO ()
printMultiPolyAnalysis mpa = do
  let layers = mpLayers mpa
      commonStr = if null (mpCommonToAll mpa)
                    then "None"
                    else intercalate ", " (map pcName (mpCommonToAll mpa))
  printf "=== Multi-Layer Polytonal Analysis (%d Layers) ===\n" (length layers)
  mapM_ (\(idx, lay) ->
           printf "  Layer %d: %-20s %s\n" idx (plLabel lay) (formatSet (plPitches lay)))
        (zip (enumFromTo 1 (length layers)) layers)
  putStrLn ""
  printf "Combined Union:    %s (Cardinality: %d/12)\n"
    (formatSet (mpUnion mpa)) (length (mpUnion mpa))
  printf "Union Set Class:   Forte %-8s (%s)  %s\n"
    (mpUnionForte mpa)
    (intercalate "," (map show (mpUnionPrime mpa)))
    (formatVector (mpUnionVector mpa))
  putStrLn $ "Universal Pivots:  " ++ commonStr
  printf "Overall Ambiguity: %d / 100\n" (mpOverallAmbiguity mpa)
  putStrLn $ "Assessment:        " ++ mpVerdict mpa

  putStrLn "\n--- Pairwise Layer Breakdown ---"
  mapM_ (\pa ->
           printf "  %-18s vs %-18s -> Score: %3d | ic1:%-2d ic6:%-2d | V7 traps: %d\n"
             (plLabel (paLayerA pa))
             (plLabel (paLayerB pa))
             (paAmbiguityScore pa)
             (length (paCrossHalfSteps pa))
             (length (paCrossTritones pa))
             (length (paDominantTraps pa)))
        (mpPairwise mpa)

  putStrLn "\n--- Global Anti-Resolution & Scale-Collapse Warnings ---"
  if null (mpParentScaleHits mpa)
    then putStrLn "Parent Scale Fit:  None (avoids single-scale absorption)"
    else putStrLn $ "Parent Scale Fit:  WARNING — all layers fit inside "
                 ++ intercalate ", " (mpParentScaleHits mpa)

  if null (mpGlobalDomTraps mpa)
    then putStrLn "Cross-Layer V7s:   None detected across combined union"
    else do
      putStrLn "Cross-Layer V7s:   WARNING — combined union forms embedded dominant 7ths:"
      mapM_ (\dt -> do
               let (thirdPc, seventhPc) = dtTritone dt
                   res3 = (thirdPc + 1) `mod` 12
                   res7 = (seventhPc - 1) `mod` 12
               printf "  * %s7 (%s) pulls toward Tonic %s (avoid resolving tritone %s-%s -> %s-%s)\n"
                 (pcName (dtDomRoot dt))
                 (intercalate "," (map pcName (dtDomPitches dt)))
                 (pcName (dtTargetTonic dt))
                 (pcName thirdPc) (pcName seventhPc)
                 (pcName res3) (pcName res7))
            (mpGlobalDomTraps mpa)
