# Theory & Workflow Guide

This guide explains the set-theory concepts implemented in `PitchClassSet` and how to combine `set-tool` and `serial-compose` in a practical composition workflow.

---

## 1. Pitch-Class Set Theory Reference (`PitchClassSet`)

### Pitch Classes & Transposition (`Tn` / `TnI`)
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

---

## 2. Combined Composition Workflows

### Workflow A: Building a Row Around a Target Trichord
1. **Pick a core trichord** (e.g., `3-5` `(0,1,6)` — minor second + tritone).
2. **Find larger supersets saturated with that trichord:**
   ```bash
   cabal run set-tool -- supersets 3-5 6
   ```
   Look for hexachords with high embedding counts (`[x3]` or `[x4]`), such as `6-7` `(0,1,2,6,7,8)` or `6-Z19` `(0,1,3,4,7,8)`.
3. **Order the pitch classes into a row and inspect its segments:**
   ```bash
   cabal run serial-compose -- -w "0,1,6,7,8,2" -r D --analyze --plain -f hex_study
   ```
4. **Open `hex_study.musicxml` in MuseScore** to refine phrasing, voicings, and register.

### Workflow B: Smooth Voice-Leading via Common-Tone Invariance
1. **Inspect `Tn` common tones** for your harmonic cell:
   ```bash
   cabal run set-tool -- 0,1,6,7,10
   ```
2. Look for high `Tn` counts (such as `T6:4`, where transposing by a tritone keeps 4 out of 5 pitches intact) to plan pivot modulations between sections.

### Workflow C: Exploring Z-Mates & Similarity Substitutions
1. **Check whether a harmonic cell has a Z-partner or `R1`/`R2` neighbors:**
   ```bash
   cabal run set-tool -- 4-Z15
   ```
2. Substitute the Z-mate (`4-Z29`) in a contrasting section to keep the exact same overall dissonance profile (`<111111>`) while shifting the melodic contour and prime form.
