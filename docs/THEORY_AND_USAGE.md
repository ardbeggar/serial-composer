# Theory & Workflow Guide

This guide explains the set-theory, melodic contour, polytonal, and neotonal concepts implemented in `PitchClassSet`, `Contour`, `Polytonal`, `Neotonal`, and `SerialComposer`, along with practical composition workflows.

---

## 1. Pitch-Class Set Theory Reference (`PitchClassSet`)

### Pitch Classes, Transposition (`Tn`), and Inversion (`TnI`)
- Pitch classes are represented modulo 12 (`0 = C`, `1 = C#/Db`, ..., `11 = B`).
- **Transposition (`Tn`):** Shifts every pitch class up by `n` semitones: `(x + n) mod 12`.
- **Inversion (`TnI`):** Reflects every pitch class around `0` and transposes by `n`: `(n - x) mod 12`.

### Multiplicative Operators (`M5` and `M7`)
- Together with `T0` (`x -> x`) and `I0` (`x -> 11x mod 12`), **`M5` (`x -> 5x mod 12`, cycle of fourths)** and **`M7` (`x -> 7x mod 12`, cycle of fifths)** are the only bijective linear operations on modulo-12 pitch-class space.
- `M5` and `M7` swap **interval-class 1** (semitones) with **interval-class 5** (perfect fourths/fifths) while keeping `ic2`, `ic3`, `ic4`, and `ic6` (tritones) invariant.

### Normal Order & Prime Form: Forte vs. Rahn
Both algorithms minimize the outer span (`last - first`) first and select the more compact zero-normalized form between `S` and `I(S)`. They differ only when breaking ties on equal outer span:
- **Allen Forte (1973) — Left-Packed:** Compares intervals from the first element to the **penultimate** (`n-2`), then `n-3`, down to the second (`1`).
- **John Rahn (1980) — Right-Packed:** Compares intervals from the first element to the **second** (`1`), then `2`, up to `n-2`.
- Out of all 208 set classes (`3-1` through `9-12`), **201 are identical** under both systems; the 7 diverging cases (`5-20`, `6-Z29`, `6-31`, `6-Z50`, `7-Z18`, `7-20`, and `8-26`) are automatically flagged by `set-tool`.

### Interval-Class Vector (`<ic1, ic2, ic3, ic4, ic5, ic6>`)
Counts the total occurrences of all six interval classes (unordered pitch-class intervals `1` through `6`) between every pair of notes in a set:
- `ic1`: Minor 2nd / Major 7th (1 or 11 semitones)
- `ic2`: Major 2nd / Minor 7th (2 or 10 semitones)
- `ic3`: Minor 3rd / Major 6th (3 or 9 semitones)
- `ic4`: Major 3rd / Minor 6th (4 or 8 semitones)
- `ic5`: Perfect 4th / Perfect 5th (5 or 7 semitones)
- `ic6`: Tritone (6 semitones)

### Z-Relations & Similarity (`R0`, `R1`, `R2`, `Rp`)
- **Z-Related Sets:** Two set classes with **identical Interval Vectors** but **different Prime Forms** (for example, `4-Z15` `(0,1,4,6)` and `4-Z29` `(0,1,3,7)`, both having `<111111>`).
- **`R1` (Maximum Similarity):** Two sets of the same cardinality share 4 identical entries in their Interval Vectors, and the remaining 2 entries are swapped (e.g., `<221131>` and `<221311>`).
- **`R2` (Strong Similarity):** Two sets share 4 identical Interval Vector entries, while the remaining 2 entries differ without swapping.
- **`R0` (Minimum Similarity / Maximum Contrast):** Two sets of the same cardinality share **zero** identical entries in their Interval Vectors (for example, `5-35` `<032140>` and `5-6` `<311221>`).
- **`Rp` (Pitch-Class Similarity):** Two sets of cardinality `N` share at least one common abstract subset of cardinality `N - 1`.

### 12-Trichord Embedding Fingerprint (`Emb(3, S)`)
For any pitch-class set `S` of cardinality `N`, counts how many of its `N choose 3` subsets belong to each of the 12 Forte trichord classes (`3-1` through `3-12`). Even Z-related sets with identical interval vectors (`6-Z19` vs. `6-Z44`) exhibit distinct trichord embedding fingerprints that explain their differing harmonic colors.

### David Lewin's Interval Function (`IFUNC(X, Y)` & `IFUNC_I(X, Y)`)
- **`IFUNC(X, Y)(n)`:** Counts all ordered pairs `(x ∈ X, y ∈ Y)` such that `(y - x) mod 12 == n` for `n = 0..11`.
- **Lewin's Common-Tone Theorem:** `IFUNC(X, Y)(n)` equals `|Tn(X) ∩ Y|`, the exact number of common tones retained when set `X` is transposed by `n` semitones onto set `Y`. Similarly, `IFUNC_I(X, Y)(n)` equals `|TnI(X) ∩ Y|`.
- The sum of all 12 entries in `IFUNC(X, Y)` always equals `|X| × |Y|`.

### Set-Complexes `K` and `Kh`
For any reference set class `T` (cardinality 3–9) and its complement `T-bar`:
- **Set-Complex `K(T, T-bar)`:** Contains every set class `S` (other than `T` and `T-bar`) that is in an abstract subset/superset relation with **`T` OR `T-bar`**.
- **Subcomplex `Kh(T, T-bar)`:** Contains every set class `S` that is in an abstract subset/superset relation with **BOTH `T` AND `T-bar`**.
- **Nexus Set:** A set class `T` that holds a `K` or `Kh` relation with every set in a target collection `{S1, S2, ...}` simultaneously.

### Hexachordal Combinatoriality
A 6-note hexachord `H` is **combinatorial** with a transformation `X(H)` if `H` and `X(H)` are disjoint (`H ∪ X(H) = {0..11}`), completing the 12-tone aggregate:
- **P / R Combinatorial:** `Tn(H) = Complement(H)` for at least one `n ∈ 0..11`.
- **I / RI Combinatorial:** `TnI(H) = Complement(H)` for at least one `n ∈ 0..11`.
- **All-Combinatorial:** Satisfies both `P/R` and `I/RI` combinatoriality (the 6 all-combinatorial hexachords are `6-1`, `6-7`, `6-8`, `6-20`, `6-32`, and `6-35`).

### Minimal Voice-Leading Distance (`L1` Taxicab Metric)
Given a concrete pitch-class set `A` and a target set class `B` of equal cardinality, `minimalVoiceLeading` tests every `Tn` and `TnI` form of `B` and solves for the optimal note-to-note bijection that minimizes total semitone motion (`sum |delta_i|`), identifying the smoothest parsimonious voice-leading paths.

### Interval Cycles & Alternating Cyclic Sets (George Perle)
- **Basic Cycle `Ck`:** An ordered orbit generated by repeatedly adding interval `k mod 12` until returning to the starting pitch class. Together, its cosets partition all 12 pitch classes into symmetrical collections (such as three `4-28` diminished-seventh orbits for `C3` or two `6-35` whole-tone orbits for `C2`).
- **Alternating Cycle `Alt(k1, k2)`:** Alternates between `+k1` and `+k2` modulo 12, generating symmetrical scales such as the **Octatonic collection (`8-28`)** via `Alt(1,2)` and the **Hexatonic collection (`6-20`)** via `Alt(1,3)`.

---

## 2. Robert Morris's Melodic Contour Theory (`Contour`)

Contour theory analyzes melodic shape independently of exact intervallic distance—tracking how a gesture remains recognizable even when intervals are expanded, compressed via `M5`/`M7`, or displaced across octaves:
- **Contour Segment (`CSeg`):** Maps each note in a melody to its relative height rank (`0` = lowest distinct pitch, `1` = next higher, ..., `maxRank` = highest), preserving unisons/ties. For example, `[B2, F3, C3, Eb3]` becomes `<0, 3, 1, 2>`.
- **Contour Transformations (`P`, `R`, `I`, `RI`):**
  - **Retrograde (`R`):** Reverses temporal order (`<2, 1, 3, 0>`).
  - **Inversion (`I`):** Flips vertical rank (`maxRank - c`, yielding `<3, 0, 2, 1>`).
  - **Retrograde-Inversion (`RI`):** Applies both (`<1, 2, 0, 3>`).
  - **Prime Contour:** The lexicographically smallest form among `{P, R, I, RI}`, used to test Contour Class equivalence (`areContoursEquivalent`).
- **Comparison Matrix (`COM-Matrix`):** A 2D grid comparing every pair of notes `(i, j)` with `+` (`c[j] > c[i]`), `-` (`c[j] < c[i]`), or `0` (`c[j] == c[i]`).

---

## 3. Polytonality & Anti-Resolution Mechanics (`Polytonal`)

When writing polytonal or bitonal music, sustaining ambiguity requires avoiding accidental tonal absorption or cadential discharge:
- **Single-Scale Collapse:** If the combined union `A ∪ B` fits inside a single 7-note diatonic (`7-35`), melodic minor (`7-34`), or harmonic minor (`7-32`) scale, the ear hears upper extensions of a single key rather than two independent tonal planes.
- **Cross-Layer Friction (`ic1` and `ic6`):** Minor-second clashes (`ic1`, e.g. `D` in Layer A against `D#` in Layer B) and cross-layer tritones (`ic6`) anchor two distinct tonal centers simultaneously.
- **Accidental Dominant-7th (`4-27`) Traps:** If combining notes from Layer A and Layer B forms `[r, r+4, r+7, r+10]`, its tritone (`r+4` and `r+10`) creates strong tonal gravity toward `(r + 5) mod 12`. `Polytonal` detects these embedded `V7` chords and warns you which target tonic to avoid resolving into.
- **Progression Anti-Resolution Hazards (`poly-step`):** Flags transitions `(A1, B1) -> (A2, B2)` where a tritone discharges by contrary half-step motion, a leading tone resolves `+1` into a target root, both layer roots lock into parallel fifths/octaves, or State 2 collapses into a single consonant triad (`3-11`).
- **Multi-Layer Polytonality (`poly-multi` & `poly-multi-partners`):** Evaluates 3 or more simultaneous tonal layers (e.g., Guitar, Bass, and Synth) to detect pairwise weak links, avoid undifferentiated `12/12` chromatic saturation, and rank non-resolving 3rd-layer additions.

---

## 4. Bridging Pitch-Class Sets & Neotonality (`Neotonal`)

Every rooted triad, seventh chord, extended/altered chord, suspended chord, or quartal stack is a concrete `PCSet`. `Neotonal` relates any arbitrary atonal pitch-class set `S` to rooted chords across four tiers without artificially prioritizing one voicing style over another:
1. **Exact Chord Matches (`C == S`):** Rooted chord symbols whose exact pitch-class collection matches `S`.
2. **Embedded Core Chords (`C subset S`):** Familiar triads or seventh chords contained inside `S`, showing which non-chord tones provide dissonant color.
3. **Completing Extension Chords (`S subset C`):** Larger extended or altered harmonies that contain all of `S`, showing which added tensions (`9`, `#11`, `b9`, etc.) complete the chord.
4. **1-Semitone Voice-Leading Neighbors (`Dist = 1`):** Equal-cardinality rooted chords reachable by shifting a single note of `S` up or down by one half-step, enabling seamless transitions between atonal cells and neotonal sonorities.

---

## 5. Serial Matrix & Rotational Techniques (`SerialComposer`)

### 4N Row-Invariance Scanner (`--invariance`)
Scans all `4N` forms (`P`, `R`, `I`, `RI`) of a matrix against `P0` to find transformations preserving the unordered pitch-class set (when `N < 12`), preserving discrete `k`-note partitions (trichords, tetrachords, hexachords), or locking pitches into identical ordinal positions.

### Stravinsky Rotational Arrays (`--rotate`)
Given an `N`-note row, generates all `N` cyclic rotations and transposes each rotation so its first pitch class matches the initial pitch of `P0` (alpha-transposition). Reading down columns `1 .. N-1` yields `N - 1` vertical chords, each analyzed with its Forte set-class name and interval vector.

---

## 6. Combined Composition Workflows

### Workflow A: Comparing Harmonic Fingerprints & Finding `R0` Contrasts
1. **Inspect the 12-trichord embedding profile and `R0` contrasting set classes of a harmony:**
   ```bash
   cabal run set-tool -- fingerprint 6-Z19
   cabal run set-tool -- fingerprint 5-35
   ```
2. Pair a set with one of its `R0` neighbors in a contrasting section to achieve maximum intervallic contrast while maintaining the same harmonic density.

### Workflow B: Tracking Melodic Gesture Across Interval Stretches (`Contour`)
1. **Extract the Contour Segment (`CSeg`) and `COM-Matrix` of a core riff motif:**
   ```bash
   cabal run set-tool -- contour B2,F3,C3,Eb3
   ```
2. **Verify whether an inverted, retrograde, or octave-displaced variation preserves the same Contour Class:**
   ```bash
   cabal run set-tool -- contour B2,F3,C3,Eb3 D4,G3,C4,A3
   ```

### Workflow C: Connecting Two Different Chords or Sections via Lewin's `IFUNC`
1. **Profile all 12 transpositions and inversions between Section A's set `X` and Section B's set `Y`:**
   ```bash
   cabal run set-tool -- ifunc 0,1,6,7 0,1,4,6
   ```
2. Choose a `Tn` or `TnI` level with maximum common tones (`Max Common Tones`) for smooth pivot voice-leading, or `Zero Overlap` for an abrupt textural break.

### Workflow D: Bridging Atonal Cells into Neotonal Harmony
1. **Take an atonal harmonic cell from your row or rotational array** (e.g., `0,1,4,6,7`).
2. **Inspect embedded triads/sevenths, completing extensions, and 1-semitone voice-leading neighbors:**
   ```bash
   cabal run set-tool -- chords 0,1,4,6,7
   ```
3. Use a 1-semitone neighbor chord to pivot smoothly from a dense atonal riff into an open neotonal cadence—or vice versa.

### Workflow E: Designing a Non-Resolving Bitonal & Polytonal Progression
1. **Find high-ambiguity bitonal partners for a base scale or chord** (e.g., `B Phrygian` or `B Minor triad`):
   ```bash
   cabal run set-tool -- poly-partners B:phrygian
   cabal run set-tool -- poly-partners B:min min
   ```
2. **Add a 3rd independent tonal layer (e.g., for Synth pad over Guitar + Bass) without triggering V7 gravity:**
   ```bash
   cabal run set-tool -- poly-multi-partners B:min F:min maj
   cabal run set-tool -- poly-multi B:min F:min D:maj
   ```
3. **Validate transitions between polychords so you never trigger an unintended cadence:**
   ```bash
   cabal run set-tool -- poly-step B:maj F:maj D:min Ab:min
   ```
