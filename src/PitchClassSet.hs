module PitchClassSet
  ( PitchClass
  , PCSet
  , IntervalVector
  , ForteEntry(..)
  , makeSet
  , transposeSet
  , invertSet
  , normalOrder
  , primeForm
  , intervalVector
  , complementSet
  , isZRelated
  , subsetsOfSize
  , commonTones
  , forteCatalog
  , forteName
  , lookupForte
  , findZMate
  , similarityRelation
  , findSimilarSets
  , formatSet
  , formatVector
  ) where

import Data.List (find, intercalate, minimumBy, nub, sort, subsequences, tails)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

type PitchClass = Int
type PCSet = [PitchClass]
type IntervalVector = [Int]

data ForteEntry = ForteEntry
  { feName   :: String
  , fePrime  :: PCSet
  , feVector :: IntervalVector
  } deriving (Show, Eq)

-- | Normalizes a list of integers into a sorted, deduplicated pitch-class set (mod 12).
makeSet :: [Int] -> PCSet
makeSet = sort . nub . map (`mod` 12)

-- | Transposes a set by @n@ semitones (Tn).
transposeSet :: Int -> PCSet -> PCSet
transposeSet n = makeSet . map (+ n)

-- | Inverts a set around 0 and transposes by @n@ (TnI).
invertSet :: Int -> PCSet -> PCSet
invertSet n = makeSet . map (\x -> n - x)

-- | Computes all cyclic rotations of a sorted list.
rotations :: [a] -> [[a]]
rotations xs = take (length xs) (iterate rotateOnce xs)
  where
    rotateOnce (y : ys) = ys ++ [y]
    rotateOnce _        = mempty

-- | Compares two cyclic rotations for Forte Normal Order.
compareSpan :: PCSet -> PCSet -> Ordering
compareSpan a b =
  let intervals (x0 : xs) = reverse [ (x - x0) `mod` 12 | x <- xs ]
      intervals _         = mempty
   in comparing intervals a b

-- | Finds the Normal Order of a pitch-class set (most compact ascending arrangement).
normalOrder :: PCSet -> PCSet
normalOrder raw =
  let s = makeSet raw
   in if length s <= 1
        then s
        else minimumBy compareSpan (rotations s)

-- | Transposes a Normal Order rotation so its first element is 0.
zeroNormalize :: PCSet -> PCSet
zeroNormalize (first : rest) = 0 : map (\x -> (x - first) `mod` 12) rest
zeroNormalize _              = mempty

-- | Computes the Forte Prime Form (most compact zero-normalized form between S and I(S)).
primeForm :: PCSet -> PCSet
primeForm raw =
  let s = makeSet raw
   in if length s <= 1
        then zeroNormalize s
        else
          let normOrig = zeroNormalize (normalOrder s)
              normInv  = zeroNormalize (normalOrder (invertSet 0 s))
           in minimumBy compareSpan [normOrig, normInv]

-- | Computes the 6-element Interval-Class Vector <ic1, ic2, ic3, ic4, ic5, ic6>.
intervalVector :: PCSet -> IntervalVector
intervalVector raw =
  let s = makeSet raw
      pairs = [ (y - x) `mod` 12 | (x : ys) <- tails s, y <- ys ]
      toIC d = min d (12 - d)
      ics = map toIC pairs
   in [ length (filter (== ic) ics) | ic <- enumFromTo 1 6 ]

-- | Returns the literal mod-12 complement of a pitch-class set (all pitches not in S).
complementSet :: PCSet -> PCSet
complementSet raw =
  let s = makeSet raw
   in [ pc | pc <- enumFromTo 0 11, pc `notElem` s ]

-- | Checks if two sets are Z-related (identical interval vectors, but different prime forms).
isZRelated :: PCSet -> PCSet -> Bool
isZRelated a b =
  intervalVector a == intervalVector b && primeForm a /= primeForm b

-- | Finds all unique subsets of size @k@ within a set, returning (literal subset, prime form).
subsetsOfSize :: Int -> PCSet -> [(PCSet, PCSet)]
subsetsOfSize k raw =
  let s = makeSet raw
      subs = filter (\sub -> length sub == k) (subsequences s)
   in [ (sub, primeForm sub) | sub <- subs ]

-- | Returns common tones between S and Tn(S) for all n in 0..11.
commonTones :: PCSet -> [(Int, Int)]
commonTones raw =
  let s = makeSet raw
   in [ (n, length [ x | x <- s, x `elem` transposeSet n s ]) | n <- enumFromTo 0 11 ]

--------------------------------------------------------------------------------
-- Complete Allen Forte Set-Class Catalog (3-1 through 9-12)
--------------------------------------------------------------------------------

forteCatalog :: [ForteEntry]
forteCatalog =
  [ ForteEntry name pf (intervalVector pf)
  | (name, pf) <- rawCatalog
  ]
  where
    rawCatalog :: [(String, PCSet)]
    rawCatalog =
      -- Trichords (12)
      [ ("3-1", [0,1,2]), ("3-2", [0,1,3]), ("3-3", [0,1,4]), ("3-4", [0,1,5])
      , ("3-5", [0,1,6]), ("3-6", [0,2,4]), ("3-7", [0,2,5]), ("3-8", [0,2,6])
      , ("3-9", [0,2,7]), ("3-10", [0,3,6]), ("3-11", [0,3,7]), ("3-12", [0,4,8])
      -- Tetrachords (29)
      , ("4-1", [0,1,2,3]), ("4-2", [0,1,2,4]), ("4-3", [0,1,3,4]), ("4-4", [0,1,2,5])
      , ("4-5", [0,1,2,6]), ("4-6", [0,1,2,7]), ("4-7", [0,1,4,5]), ("4-8", [0,1,5,6])
      , ("4-9", [0,1,6,7]), ("4-10", [0,2,3,5]), ("4-11", [0,1,3,5]), ("4-12", [0,2,3,6])
      , ("4-13", [0,1,3,6]), ("4-14", [0,2,3,7]), ("4-Z15", [0,1,4,6]), ("4-16", [0,1,5,7])
      , ("4-17", [0,3,4,7]), ("4-18", [0,1,4,7]), ("4-19", [0,1,4,8]), ("4-20", [0,1,5,8])
      , ("4-21", [0,2,4,6]), ("4-22", [0,2,4,7]), ("4-23", [0,2,5,7]), ("4-24", [0,2,4,8])
      , ("4-25", [0,2,6,8]), ("4-26", [0,3,5,8]), ("4-27", [0,2,5,8]), ("4-28", [0,3,6,9])
      , ("4-Z29", [0,1,3,7])
      -- Pentachords (38)
      , ("5-1", [0,1,2,3,4]), ("5-2", [0,1,2,3,5]), ("5-3", [0,1,2,4,5]), ("5-4", [0,1,2,3,6])
      , ("5-5", [0,1,2,3,7]), ("5-6", [0,1,2,5,6]), ("5-7", [0,1,2,6,7]), ("5-8", [0,2,3,4,6])
      , ("5-9", [0,1,2,4,6]), ("5-10", [0,1,3,4,6]), ("5-11", [0,2,3,4,7]), ("5-Z12", [0,1,3,5,6])
      , ("5-13", [0,1,2,4,8]), ("5-14", [0,1,2,5,7]), ("5-15", [0,1,2,6,8]), ("5-16", [0,1,3,4,7])
      , ("5-Z17", [0,1,3,4,8]), ("5-Z18", [0,1,4,5,7]), ("5-19", [0,1,3,6,7]), ("5-20", [0,1,3,7,8])
      , ("5-21", [0,1,4,5,8]), ("5-22", [0,1,4,7,8]), ("5-23", [0,2,3,5,7]), ("5-24", [0,1,3,5,7])
      , ("5-25", [0,2,3,5,8]), ("5-26", [0,2,4,5,8]), ("5-27", [0,1,3,5,8]), ("5-28", [0,2,3,6,8])
      , ("5-29", [0,1,3,6,8]), ("5-30", [0,1,4,6,8]), ("5-31", [0,1,3,6,9]), ("5-32", [0,1,4,6,9])
      , ("5-33", [0,2,4,6,8]), ("5-34", [0,2,4,6,9]), ("5-35", [0,2,4,7,9]), ("5-Z36", [0,1,2,4,7])
      , ("5-Z37", [0,3,4,5,8]), ("5-Z38", [0,1,2,5,8])
      -- Hexachords (50)
      , ("6-1", [0,1,2,3,4,5]), ("6-2", [0,1,2,3,4,6]), ("6-Z3", [0,1,2,3,5,6])
      , ("6-Z4", [0,1,2,4,5,6]), ("6-5", [0,1,2,3,6,7]), ("6-Z6", [0,1,2,5,6,7])
      , ("6-7", [0,1,2,6,7,8]), ("6-8", [0,2,3,4,5,7]), ("6-9", [0,1,2,3,5,7])
      , ("6-Z10", [0,1,3,4,5,7]), ("6-Z11", [0,1,2,4,5,7]), ("6-Z12", [0,1,2,4,6,7])
      , ("6-Z13", [0,1,3,4,6,7]), ("6-14", [0,1,3,4,5,8]), ("6-15", [0,1,2,4,5,8])
      , ("6-16", [0,1,4,5,6,8]), ("6-Z17", [0,1,2,4,7,8]), ("6-18", [0,1,2,5,7,8])
      , ("6-Z19", [0,1,3,4,7,8]), ("6-20", [0,1,4,5,8,9]), ("6-21", [0,2,3,4,6,8])
      , ("6-22", [0,1,2,4,6,8]), ("6-Z23", [0,2,3,5,6,8]), ("6-Z24", [0,1,3,4,6,8])
      , ("6-Z25", [0,1,3,5,6,8]), ("6-Z26", [0,1,3,5,7,8]), ("6-27", [0,1,3,4,6,9])
      , ("6-Z28", [0,1,3,5,6,9]), ("6-Z29", [0,1,3,6,8,9]), ("6-30", [0,1,3,6,7,9])
      , ("6-31", [0,1,3,5,8,9]), ("6-32", [0,2,4,5,7,9]), ("6-33", [0,2,3,5,7,9])
      , ("6-34", [0,1,3,5,7,9]), ("6-35", [0,2,4,6,8,10]), ("6-Z36", [0,1,2,3,4,7])
      , ("6-Z37", [0,1,2,3,4,8]), ("6-Z38", [0,1,2,3,7,8]), ("6-Z39", [0,2,3,4,5,8])
      , ("6-Z40", [0,1,2,3,5,8]), ("6-Z41", [0,1,2,3,6,8]), ("6-Z42", [0,1,2,3,6,9])
      , ("6-Z43", [0,1,2,5,6,8]), ("6-Z44", [0,1,2,5,6,9]), ("6-Z45", [0,2,3,4,6,9])
      , ("6-Z46", [0,1,2,4,6,9]), ("6-Z47", [0,1,2,4,7,9]), ("6-Z48", [0,1,2,5,7,9])
      , ("6-Z49", [0,1,3,4,7,9]), ("6-Z50", [0,1,4,6,7,9])
      -- Septachords (38)
      , ("7-1", [0,1,2,3,4,5,6]), ("7-2", [0,1,2,3,4,5,7])
      , ("7-3", [0,1,2,3,4,5,8]), ("7-4", [0,1,2,3,4,6,7])
      , ("7-5", [0,1,2,3,5,6,7]), ("7-6", [0,1,2,3,4,7,8])
      , ("7-7", [0,1,2,3,6,7,8]), ("7-8", [0,2,3,4,5,6,8])
      , ("7-9", [0,1,2,3,4,6,8]), ("7-10", [0,1,2,3,4,6,9])
      , ("7-11", [0,1,3,4,5,6,8]), ("7-Z12", [0,1,2,3,4,7,9])
      , ("7-13", [0,1,2,4,5,6,8]), ("7-14", [0,1,2,3,5,7,8])
      , ("7-15", [0,1,2,4,6,7,8]), ("7-16", [0,1,2,3,5,6,9])
      , ("7-Z17", [0,1,2,4,5,6,9]), ("7-Z18", [0,1,2,3,5,8,9])
      , ("7-19", [0,1,2,3,6,7,9]), ("7-20", [0,1,2,4,7,8,9])
      , ("7-21", [0,1,2,4,5,8,9]), ("7-22", [0,1,2,5,6,8,9])
      , ("7-23", [0,2,3,4,5,7,9]), ("7-24", [0,1,2,3,5,7,9])
      , ("7-25", [0,2,3,4,6,7,9]), ("7-26", [0,1,3,4,5,7,9])
      , ("7-27", [0,1,2,4,5,7,9]), ("7-28", [0,1,3,5,6,7,9])
      , ("7-29", [0,1,2,4,6,7,9]), ("7-30", [0,1,2,4,6,8,9])
      , ("7-31", [0,1,3,4,6,7,9]), ("7-32", [0,1,3,4,6,8,9])
      , ("7-33", [0,1,2,4,6,8,10]), ("7-34", [0,1,3,4,6,8,10])
      , ("7-35", [0,1,3,5,6,8,10]), ("7-Z36", [0,1,2,3,5,6,8])
      , ("7-Z37", [0,1,3,4,5,7,8]), ("7-Z38", [0,1,2,4,5,7,8])
      -- Octachords (29)
      , ("8-1", [0,1,2,3,4,5,6,7]), ("8-2", [0,1,2,3,4,5,6,8])
      , ("8-3", [0,1,2,3,4,5,6,9]), ("8-4", [0,1,2,3,4,5,7,8])
      , ("8-5", [0,1,2,3,4,6,7,8]), ("8-6", [0,1,2,3,5,6,7,8])
      , ("8-7", [0,1,2,3,4,5,8,9]), ("8-8", [0,1,2,3,4,7,8,9])
      , ("8-9", [0,1,2,3,6,7,8,9]), ("8-10", [0,2,3,4,5,6,7,9])
      , ("8-11", [0,1,2,3,4,5,7,9]), ("8-12", [0,1,3,4,5,6,7,9])
      , ("8-13", [0,1,2,3,4,6,7,9]), ("8-14", [0,1,2,4,5,6,7,9])
      , ("8-Z15", [0,1,2,3,4,6,8,9]), ("8-16", [0,1,2,3,5,7,8,9])
      , ("8-17", [0,1,3,4,5,6,8,9]), ("8-18", [0,1,2,3,5,6,8,9])
      , ("8-19", [0,1,2,4,5,6,8,9]), ("8-20", [0,1,2,4,5,7,8,9])
      , ("8-21", [0,1,2,3,4,6,8,10]), ("8-22", [0,1,2,3,5,6,8,10])
      , ("8-23", [0,1,2,3,5,7,8,10]), ("8-24", [0,1,2,4,5,6,8,10])
      , ("8-25", [0,1,2,4,6,7,8,10]), ("8-26", [0,1,2,4,5,7,9,10])
      , ("8-27", [0,1,2,4,5,7,8,10]), ("8-28", [0,1,3,4,6,7,9,10])
      , ("8-Z29", [0,1,2,3,5,6,7,9])
      -- Nonachords (12)
      , ("9-1", [0,1,2,3,4,5,6,7,8]), ("9-2", [0,1,2,3,4,5,6,7,9])
      , ("9-3", [0,1,2,3,4,5,6,8,9]), ("9-4", [0,1,2,3,4,5,7,8,9])
      , ("9-5", [0,1,2,3,4,6,7,8,9]), ("9-6", [0,1,2,3,4,5,6,8,10])
      , ("9-7", [0,1,2,3,4,5,7,8,10]), ("9-8", [0,1,2,3,4,6,7,8,10])
      , ("9-9", [0,1,2,3,5,6,7,8,10]), ("9-10", [0,1,2,3,4,6,7,9,10])
      , ("9-11", [0,1,2,3,5,6,7,9,10]), ("9-12", [0,1,2,4,5,6,8,9,10])
      ]

-- | Looks up the Forte name (e.g. "3-5", "4-Z15") for any pitch-class set.
forteName :: PCSet -> String
forteName raw =
  let pf = primeForm raw
   in case length pf of
        0 -> "0-1 (Empty)"
        1 -> "1-1 (Monad)"
        2 -> case pf of
               [0,1] -> "2-1 (ic1)"
               [0,2] -> "2-2 (ic2)"
               [0,3] -> "2-3 (ic3)"
               [0,4] -> "2-4 (ic4)"
               [0,5] -> "2-5 (ic5)"
               [0,6] -> "2-6 (ic6)"
               _     -> "2-?"
        10 -> case primeForm (complementSet pf) of
                [0,1] -> "10-1"
                [0,2] -> "10-2"
                [0,3] -> "10-3"
                [0,4] -> "10-4"
                [0,5] -> "10-5"
                [0,6] -> "10-6"
                _     -> "10-?"
        11 -> "11-1 (Undecachord)"
        12 -> "12-1 (Aggregate)"
        _  -> fromMaybe "Unknown" (feName <$> find (\e -> fePrime e == pf) forteCatalog)

-- | Finds a catalog entry by Forte name (case-insensitive, e.g. "4-z15" or "4-Z15").
lookupForte :: String -> Maybe ForteEntry
lookupForte query =
  let norm = map toUpperChar query
   in find (\e -> map toUpperChar (feName e) == norm) forteCatalog
  where
    toUpperChar c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- | Finds the Z-related partner (same Interval Vector, different Prime Form), if one exists.
findZMate :: PCSet -> Maybe ForteEntry
findZMate raw =
  let pf = primeForm raw
      iv = intervalVector pf
   in find (\e -> feVector e == iv && fePrime e /= pf) forteCatalog

--------------------------------------------------------------------------------
-- Interval-Vector Similarity Relations (Forte: R1, R2, Rp)
--------------------------------------------------------------------------------

-- | Evaluates Forte's similarity relations between two sets of the same cardinality.
similarityRelation :: PCSet -> PCSet -> [String]
similarityRelation rawA rawB =
  let pfA = primeForm rawA
      pfB = primeForm rawB
      ivA = intervalVector pfA
      ivB = intervalVector pfB
   in if length pfA /= length pfB || pfA == pfB
        then mempty
        else
          let diffs = [ (a, b) | (a, b) <- zip ivA ivB, a /= b ]
              r1r2 = case diffs of
                       [(a1, b1), (a2, b2)] ->
                         if a1 == b2 && b1 == a2 then ["R1"] else ["R2"]
                       _ -> mempty
              subsA = map snd (subsetsOfSize (length pfA - 1) pfA)
              subsB = map snd (subsetsOfSize (length pfB - 1) pfB)
              rp = if any (`elem` subsB) subsA then ["Rp"] else mempty
           in r1r2 ++ rp

-- | Finds all set classes of the same cardinality in the Forte catalog sharing R1, R2, or Rp.
findSimilarSets :: PCSet -> [(ForteEntry, [String])]
findSimilarSets raw =
  let pf = primeForm raw
      card = length pf
      candidates = filter (\e -> length (fePrime e) == card && fePrime e /= pf) forteCatalog
   in [ (entry, rels)
      | entry <- candidates
      , let rels = similarityRelation pf (fePrime entry)
      , not (null rels)
      ]

--------------------------------------------------------------------------------
-- Formatting Helpers
--------------------------------------------------------------------------------

formatSet :: PCSet -> String
formatSet xs = "[" ++ intercalate "," (map show xs) ++ "]"

formatVector :: IntervalVector -> String
formatVector iv = "<" ++ concatMap show iv ++ ">"
