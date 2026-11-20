# Theory & Workflow Guide

This guide explains the set-theory concepts implemented in `PitchClassSet` and `SerialComposer`, along with practical workflows combining `set-tool` and `serial-compose`.

---

## 1. Pitch-Class Set Theory Reference (`PitchClassSet`)

### Pitch Classes, Transposition (`Tn`), and Inversion (`TnI`)
- Pitch classes are represented modulo 12 (`0 = C`, `1 = C#/Db`, ..., `11 = B`).
- **Transposition (`Tn`):** Shifts every pitch class up by `n` semitones: `(x + n) mod 12`.
- **Inversion (`TnI`):** Reflects every pitch class around `0` and transposes by `n`: `(n - x) mod 12`.

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

---

## 2. Combined Composition Workflows

### Workflow A: Building a Derived 12-Tone Row from a Core Cell
1. **Pick a core trichord or tetrachord** (e.g., `3-5` `(0,1,6)` — minor second + tritone).
2. **Generate a 12-tone row where all 4 discrete trichords are `Tn`/`TnI` forms of `3-5`:**
   ```bash
   cabal run serial-compose -- --derive 0,1,6 -r B --seed 42 --analyze --plain -f derived_3_5
   ```
3. **Open `derived_3_5.musicxml` in MuseScore** to shape register, rhythm, and phrasing.

### Workflow B: Unifying Multi-Section Harmony with Nexus Sets
1. **Collect the main harmonic cells across your sections** (e.g., `3-5`, `4-Z15`, and `5-19`).
2. **Find all `K` and `Kh` nexus sets connecting them:**
   ```bash
   cabal run set-tool -- nexus 3-5 4-Z15 5-19
   ```
3. Use a strict `Kh` nexus hexachord (such as `6-Z19` or `6-Z44`) to bridge sections while preserving subset/superset coherence.

### Workflow C: Voice-Leading & Combinatorial Aggregate Counterpoint
1. **Check `Tn` and `TnI` common-tone invariance** for a harmonic cell:
   ```bash
   cabal run set-tool -- 0,1,6,7
   ```
   Look at both `Tn` and `TnI` vectors (e.g., `T6:4` or `T1I:4`) to find transpositions or inversions that retain the most shared pitches.
2. **Verify exact transformations between two specific voicings:**
   ```bash
   cabal run set-tool -- 0,1,6,7 2,3,8,9
   ```
3. **Test a hexachord for aggregate counterpoint** (e.g., Guitar playing `P0` while Bass plays an `I`-combinatorial inversion `TnI` to complete all 12 pitch classes):
   ```bash
   cabal run set-tool -- comb 6-7
   ```
