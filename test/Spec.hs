module Main (main) where

import Control.Monad (unless)
import Neotonal
import Contour
import PitchClassSet
import Polytonal
import SerialComposer
import System.Exit (exitFailure)
import System.Random (mkStdGen)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $ do
    putStrLn $ "FAIL [" ++ label ++ "]: expected " ++ show expected ++ ", got " ++ show actual
    exitFailure

assertTrue :: String -> Bool -> IO ()
assertTrue label cond =
  unless cond $ do
    putStrLn $ "FAIL [" ++ label ++ "]: condition was False"
    exitFailure

-- | Helper to build integer lists without square-bracket syntax.
pcs :: [Int] -> PCSet
pcs = makeSet

main :: IO ()
main = do
  putStrLn "Running SerialComposer & PitchClassSet test suite..."

  -- 1. Catalog completeness (208 set classes from 3-1 through 9-12)
  assertEqual "Forte catalog count" 208 (length forteCatalog)

  -- 2. Forte vs. Rahn prime form comparison
  let set520   = pcs (0 : 1 : 3 : 7 : 8 : mempty)
      forte520 = 0 : 1 : 5 : 6 : 8 : mempty
      rahn520  = 0 : 1 : 3 : 7 : 8 : mempty
  assertEqual "Forte 5-20 name" "5-20" (forteName set520)
  assertEqual "Forte 5-20 prime form" forte520 (primeForm set520)
  assertEqual "Rahn 5-20 prime form" rahn520 (rahnPrimeForm set520)

  -- 3. Interval vectors & Z-relations (4-Z15 and 4-Z29)
  let z15 = pcs (0 : 1 : 4 : 6 : mempty)
      z29 = pcs (0 : 1 : 3 : 7 : mempty)
      ivAllOne = 1 : 1 : 1 : 1 : 1 : 1 : mempty
  assertEqual "4-Z15 interval vector" ivAllOne (intervalVector z15)
  assertTrue "4-Z15 and 4-Z29 are Z-related" (isZRelated z15 z29)
  assertEqual "Find Z-mate of 4-Z15" (Just "4-Z29") (feName <$> findZMate z15)

  -- 4. M5 / M7 multiplicative bijection swapping ic1 and ic5
  let trichord35  = pcs (0 : 1 : 6 : mempty)
      chromatic31 = pcs (0 : 1 : 2 : mempty)
  assertEqual "M5(0,1,6) -> (0,5,6) -> 3-5" "3-5" (forteName (m5Set trichord35))
  assertEqual "M5(0,1,2) -> (0,5,10) -> 3-9" "3-9" (forteName (m5Set chromatic31))

  -- 5. Hexachordal combinatoriality (6-7 is All-Combinatorial; 6-Z19 is Non-Combinatorial)
  let hex67   = pcs (0 : 1 : 2 : 6 : 7 : 8 : mempty)
      hex6Z19 = pcs (0 : 1 : 3 : 4 : 7 : 8 : mempty)
  case analyzeCombinatoriality hex67 of
    Nothing -> assertTrue "6-7 combinatoriality Just" False
    Just res -> do
      assertTrue "6-7 has P/R combinatorial levels" (not (null (crPTransLevels res)))
      assertTrue "6-7 has I/RI combinatorial levels" (not (null (crITransLevels res)))

  case analyzeCombinatoriality hex6Z19 of
    Nothing -> assertTrue "6-Z19 combinatoriality Just" False
    Just res -> do
      assertTrue "6-Z19 has no P/R levels" (null (crPTransLevels res))
      assertTrue "6-Z19 has no I/RI levels" (null (crITransLevels res))

  -- 6. Interval cycles & alternating cycles
  assertEqual "C3 cosets count" 3 (length (cycleCosets 3 0))
  assertEqual "Alt(1,3) hexatonic set class" "6-20" (forteName (makeSet (alternatingCycle (1, 3) 0)))
  assertEqual "Alt(1,2) octatonic set class" "8-28" (forteName (makeSet (alternatingCycle (1, 2) 0)))

  -- 7. Derived & All-Interval 12-tone row generators
  case deriveRow12 trichord35 (mkStdGen 42) of
    (Nothing, _) -> assertTrue "deriveRow12 3-5 succeeds" False
    (Just dRow, _) -> do
      assertEqual "Derived row length" 12 (length dRow)
      assertEqual "Derived row covers all 12 PCs" 12 (length (makeSet dRow))
      let trichords = partitionRow 3 dRow
      assertTrue "All 4 discrete trichords belong to 3-5"
        (all (\seg -> forteName seg == "3-5") trichords)

  let (aiRow, _) = allIntervalRow12 (mkStdGen 7)
  assertEqual "All-interval row covers 12 PCs" 12 (length (makeSet aiRow))
  let adjInts = [ (b - a) `mod` 12 | (a, b) <- zip aiRow (drop 1 aiRow) ]
  assertEqual "All-interval row has 11 distinct intervals" 11 (length (makeSet adjInts))

  -- 8. Polytonal ambiguity, scale collapse, and V7 traps
  case (parseLayerSpec "C:maj", parseLayerSpec "D:min") of
    (Just cMaj, Just dMin) -> do
      let pa = analyzeBitonal cMaj dMin
      assertTrue "C:maj + D:min collapses into single parent scale" (not (null (paParentScaleHits pa)))
    _ -> assertTrue "parseLayerSpec C:maj / D:min" False

  case (parseLayerSpec "B:min", parseLayerSpec "F:min") of
    (Just bMin, Just fMin) -> do
      let pa = analyzeBitonal bMin fMin
      assertTrue "B:min + F:min avoids single-scale collapse" (null (paParentScaleHits pa))
      assertTrue "B:min + F:min has zero V7 traps" (null (paDominantTraps pa))
      assertTrue "B:min + F:min achieves high ambiguity (>= 70)" (paAmbiguityScore pa >= 70)
    _ -> assertTrue "parseLayerSpec B:min / F:min" False

  -- 9. Neotonal chord vocabulary matching
  let dim7Set = pcs (0 : 3 : 6 : 9 : mempty)
      neoDim  = analyzeNeotonal dim7Set
  assertEqual "dim7 exact matches count (4 symmetric roots)" 4 (length (nrExact neoDim))

  let tet418 = pcs (0 : 1 : 4 : 7 : mempty)
      neo418 = analyzeNeotonal tet418
  assertTrue "4-18 embeds core triads (C# dim)" (not (null (nrSubsets neo418)))
  assertTrue "4-18 has completing extension supersets (C 7b9)" (not (null (nrSupersets neo418)))

  -- 10. Lewin's IFUNC(X, Y) Common-Tone Theorem (sum of vector == |X| * |Y|)
  let repIF = analyzeIFunc z15 z29
      totalDirected = sum (map snd (ifTransTable repIF))
  assertEqual "IFUNC(4-Z15, 4-Z29) total directed intervals == 4 * 4" 16 totalDirected

  -- 11. Robert Morris Melodic Contour Segments & Prime Equivalence
  let melA = 47 : 53 : 48 : 51 : mempty -- B2, F3, C3, Eb3 -> <0,3,1,2>
      melB = 62 : 55 : 60 : 57 : mempty -- D4, G3, C4, A3  -> <3,0,2,1> (Inversion!)
  assertEqual "CSeg of B2,F3,C3,Eb3" (0 : 3 : 1 : 2 : mempty) (toCSeg melA)
  assertTrue "melA and melB belong to same Contour Class (I-related)"
    (areContoursEquivalent melA melB)

  -- 12. Forte R0 similarity & 12-Trichord Embedding Fingerprint
  let pent535 = pcs (0 : 2 : 4 : 7 : 9 : mempty) -- IV <032140> (Pentatonic)
      pent506 = pcs (0 : 1 : 2 : 5 : 6 : mempty) -- IV <311221>
  assertTrue "5-35 and 5-6 are R0-related" ("R0" `elem` similarityRelation pent535 pent506)
  assertTrue "5-6 appears in findR0Sets of 5-35"
    (any (\e -> feName e == "5-6") (findR0Sets pent535))

  let prof535 = trichordProfile pent535
  assertEqual "12-trichord profile has 12 slots" 12 (length prof535)
  assertEqual "5-35 total embedded trichords == 5 choose 3 (10)" 10 (sum (map snd prof535))

  -- 13. Complete Tn / TnI orbit enumeration
  assertEqual "allTranspositions returns 12 forms" 12 (length (allTranspositions trichord35))
  assertEqual "allInversions returns 12 forms" 12 (length (allInversions trichord35))
  assertEqual "T6 of (0,1,6) == (0,6,7)"
    (0 : 6 : 7 : mempty)
    (snd (allTranspositions trichord35 !! 6))

  putStrLn "All tests passed!"
