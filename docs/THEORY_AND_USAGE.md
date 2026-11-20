# Theory & Workflow Guide

This guide explains the set-theory concepts implemented in `PitchClassSet` and `SerialComposer`, along with practical workflows combining `set-tool` and `serial-compose`.

---

## 1. Pitch-Class Set Theory Reference (`PitchClassSet`)

### Pitch Classes, Transposition (`Tn`), and Inversion (`TnI`)
- Pitch classes are represented modulo 12 (`0 = C`, `1 = C#/Db`, ..., `11 = B`).
- **Transposition (`Tn`):** Shifts every pitch class up by `n` semitones: `(x + n) mod 12`.
- **Inversion (`TnI`):** Reflects every pitch class around `0` and transposes by `n`: `(n - x) mod 12`.

### Multiplicative Operators (`M5` and `M7`)
- Together with `T0` (`x -> x`) and `I0` (`x -> 11x mod 12`), **`M5` (`x -> 5x mod 12`, cycle of fourths)** and **`M7` (`x -> 7x mod 12`, cycle of fifths)** are the only bijective linear operations on modulo-12 pitch-class space.
- `M5` and `M7` swap **interval-class 1** (semitones) with **interval-class 5** (perfect fourths/fifths) while keeping `ic2`, `ic3`, `ic4`, and `ic6` (tritones) invariant.

### Normal Order & Prime Form
- **Normal Order:** The most compact ascending cyclic rotation of a pitch-class set (smallest outer interval span, breaking ties by packing intervals to the left).
- **Prime Form:** The most compact zero-normalized form (`0` as the first element) chosen between a set's Normal Order and its Inversion's Normal Order. Two sets share the same **Forte Name** (e.g., `3-5`, `4-Z15`) if and only if they have the same Prime Form.

### Interval-Class Vector (`<ic1, ic2, ic3, ic4, ic5, ic6>`)
Counts the total occurrences of all six interval classes (unordered pitch-class intervals `1` through `6`) between every pair of notes in a set:
- `ic1`: Minor 2nd / Major 7th (1 or 11 semitones)
- `ic2`: Major 2nd / Minor 7th (2 or 10 semitones)
- `ic3`: Minor 3rd / Major 6th (3 or 9 semitones)
- `ic4`: Major 3rd / Minor 6th (4 or 8 semitones)
- `ic5`: Perfect 4th / Perfect 5th (5 or 7 semitones)
- `ic6`: Tritone (6 semitones)

### Z-Relations & Similarity (`R1`, `R2`, `Rp`)
- **Z-Related Sets:** Two set classes with **identical Interval Vectors** but **different Prime Forms** (for example, `4-Z15` `(0,1,4,6)` and `4-Z29` `(0,1,3,7)`, both having `<111111>`).
- **`R1` (Maximum Similarity):** Two sets of the same cardinality share 4 identical entries in their Interval Vectors, and the remaining 2 entries are swapped (e.g., `<221131>` and `<221311>`).
- **`R2` (Strong Similarity):** Two sets share 4 identical Interval Vector entries, while the remaining 2 entries differ without swapping.
- **`Rp` (Pitch-Class Similarity):** Two sets of cardinality `N` share at least one common abstract subset of cardinality `N - 1`.

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

---

## 2. Serial Matrix & Rotational Techniques (`SerialComposer`)

### 4N Row-Invariance Scanner (`--invariance`)
Scans all `4N` forms (`P`, `R`, `I`, `RI`) of a matrix against `P0` to find:
- Transformations preserving the exact unordered pitch-class set (when `N < 12`).
- Transformations preserving discrete `k`-note partitions (trichords, tetrachords, hexachords) either in exact slot order or permuted across slots.
- Transformations locking pitches into identical ordinal positions.

### Stravinsky Rotational Arrays (`--rotate`)
Given an `N`-note row, generates all `N` cyclic rotations and transposes each rotation so its first pitch class matches the initial pitch of `P0` (alpha-transposition). Reading down columns `1 .. N-1` yields `N - 1` vertical chords, each analyzed with its Forte set-class name and interval vector.

---

## 3. Combined Composition Workflows

### Workflow A: Building & Transforming a Derived 12-Tone Row
1. **Pick a core trichord or tetrachord** (e.g., `3-5` `(0,1,6)` — minor second + tritone).
2. **Generate a 12-tone derived row, inspect its segments, and scan all 48 matrix forms for segment-preserving counter-rows:**
   ```bash
   cabal run serial-compose -- --derive 0,1,6 -r B --seed 42 --analyze --invariance --plain -f derived_3_5
   ```
3. **Create a contrasting open quartal/quintal variation using `--m5`:**
   ```bash
   cabal run serial-compose -- --derive 0,1,6 -r B --seed 42 --m5 --analyze --plain -f derived_m5
   ```

### Workflow B: Generating Harmony from a Short Row via Rotational Arrays
1. **Start with a 5- or 6-note row** (e.g., `[0,1,6,7,10]` in Drop B).
2. **Generate the Stravinsky rotational array and inspect its vertical column chords:**
   ```bash
   cabal run serial-compose -- -w "0,1,6,7,10" -r B --rotate --invariance
   ```
3. Use the vertical columns as a chord progression and the invariant `P`/`R`/`I`/`RI` forms as melodic counter-lines.

### Workflow C: Parsimonious Voice-Leading & Nexus Set Bridging
1. **Find the smoothest voice-leading from one chord to another set class:**
   ```bash
   cabal run set-tool -- voicelead 0,1,6,7 4-Z15
   ```
2. **Find a `Kh` nexus hexachord that unifies multiple cells across sections:**
   ```bash
   cabal run set-tool -- nexus 3-5 4-Z15 5-19
   ```
