# SerialComposer

`SerialComposer` is a pure Haskell library suite and set of command-line tools (`serial-compose` and `set-tool`) for **N × N serial matrix generation**, **derived and all-interval row construction**, **Stravinsky rotational arrays**, **Allen Forte & John Rahn pitch-class set theory analysis**, **interval cycles**, **2-layer and multi-layer polytonal ambiguity & anti-resolution analysis**, and **MusicXML / LilyPond score export**.

See [docs/THEORY_AND_USAGE.md](docs/THEORY_AND_USAGE.md) for mathematical definitions and combined composition workflows.

---

## Project Structure

```text
serial-composer/
├── serial-composer.cabal
├── README.md
├── docs/
│   └── THEORY_AND_USAGE.md
├── app/
│   ├── Main.hs           (serial-compose CLI)
│   └── SetTool.hs        (set-tool CLI)
└── src/
    ├── SerialComposer.hs (N x N matrix, invariance, rotational arrays, M5/M7, score export)
    ├── PitchClassSet.hs  (Forte/Rahn prime forms, catalog, K/Kh, combinatoriality, cycles)
    └── Polytonal.hs      (2-layer & 3+ layer bitonal/polytonal friction, V7 traps, partner ranker)
```

---

## Requirements

- **GHC** (9.8+ / 9.10+ compatible, compiles cleanly under `-Wall` and `-Wx-partial`)
- **Cabal** (`cabal-install` 3.0+)
- **Optional:** [MuseScore](https://musescore.org) for opening `.musicxml` files, or [LilyPond](https://lilypond.org) for compiling `.ly` files to PDF and MIDI.

---

## 1. Building & Installing

Build both libraries and CLI executables:

```bash
cabal build
```

Optional: Install `serial-compose` and `set-tool` to your local Cabal bin directory (`~/.cabal/bin` or `~/.local/bin`):

```bash
cabal install
```

Generate local Haddock API documentation:

```bash
cabal haddock --haddock-all
```

---

## 2. Serial Matrix & Score Generator (`serial-compose`)

Run via `cabal run serial-compose -- [OPTIONS...]` or using the installed `serial-compose` binary.

### Available Flags

| Short | Long | Argument | Description |
| :--- | :--- | :--- | :--- |
| `-w` | `--row` | `LIST` | Custom comma-separated notes or integers, e.g. `C,Db,Gb,G,Bb` or `0,1,6,7,10` (overrides `--length`) |
| `-d` | `--derive` | `CELL` | Generate a 12-tone derived row from a 3- or 4-note cell (e.g. `0,1,6` or `C,Db,Gb`) |
| `-i` | `--all-interval` | | Generate a 12-tone all-interval row (contains every interval `1..11` once) |
| | `--m5` | | Apply `M5` cycle-of-fourths transform (`x -> 5x mod m`) to the row |
| | `--m7` | | Apply `M7` cycle-of-fifths transform (`x -> 7x mod m`) to the row |
| `-n` | `--length` | `INT` | Number of unique pitches in random row (default: `6`) |
| `-m` | `--mod` | `INT` | Modulus / chromatic division (default: `12`) |
| `-s` | `--seed` | `INT` | Deterministic integer RNG seed (default: random system seed) |
| `-r` | `--root` | `NOTE` | Root note name: `C`, `Db`, `D`, `Eb`, `E`, `F`, `Gb`, `G`, `Ab`, `A`, `Bb`, `B` (default: `C`) |
| `-o` | `--octave` | `INT` | Override base octave for guitar (default: auto based on root) |
| `-p` | `--part` | `MODE` | Instrument mode: `guitar`, `bass`, or `duo` (default: `duo`) |
| | `--plain` | | Use plain quarter notes without rhythm/octave displacement |
| `-a` | `--analyze` | | Print Forte pitch-class set segmentation of `P0` (requires `--mod 12`) |
| `-v` | `--invariance` | | Scan all `4N` matrix forms for segment, pitch-set, and positional invariance |
| `-R` | `--rotate` | | Print Stravinsky rotational array and vertical column Forte analysis |
| `-t` | `--tempo` | `BPM` | Tempo in BPM for LilyPond MIDI export (default: `75`) |
| `-f` | `--out` | `PREFIX` | Output filename prefix without extension (default: `serial_output`) |
| `-h` | `--help` | | Show help message |

### Examples

**Generate a random 5-tone Drop B Guitar + Bass counterpoint score with a fixed seed:**

```bash
cabal run serial-compose -- --length 5 --root B --seed 42 --tempo 68 --out doom_b
```

**Generate a 12-tone row derived from trichord `3-5` (`0,1,6`), inspect segments and `4N` invariance:**

```bash
cabal run serial-compose -- --derive 0,1,6 -r B --seed 42 --analyze --invariance --plain -f derived_3_5
```

**Transform a row through `M5` (circle of fourths) and inspect its Stravinsky rotational array:**

```bash
cabal run serial-compose -- -w "0,1,6,7,10" -r D --m5 --rotate --analyze
```

---

## 3. Pitch-Class Set Theory & Polytonality Tool (`set-tool`)

`set-tool` combines Allen Forte's *The Structure of Atonal Music*, John Rahn's prime form algorithm, George Perle's interval cycles, and 2-layer / multi-layer polytonal anti-resolution analysis.

### Set-Theory Commands (`PitchClassSet`)

| Command | Description |
| :--- | :--- |
| `set-tool <set-or-forte-name>` | Full analysis: Normal Order & Prime Form (Forte & Rahn), Interval Vector, Complement, Z-Mate, `M5`/`M7` transforms, `Tn` & `TnI` common tones, internal symmetry, `R1`/`R2`/`Rp` similar sets, and embedded trichords |
| `set-tool <set-1> <set-2>` | Compare two sets: `Tn`/`TnI` equivalence, exact `Tn`/`TnI` mapping (`A -> B`), Z-relation, and `R1`/`R2`/`Rp` similarity |
| `set-tool voicelead <src> <target>` | Find the most parsimonious `Tn`/`TnI` voice-leadings from `<src>` to `<target>` with minimum total semitone displacement |
| `set-tool supersets <set> [card]` | Find all Forte catalog supersets containing `<set>` (optionally filtered to cardinality `card`), with embedding multiplicity `[xN]` |
| `set-tool subsets <set> [card]` | Find all Forte catalog subsets contained in `<set>` (optionally filtered to cardinality `card`), with embedding multiplicity `[xN]` |
| `set-tool complex <set>` | List all members of Forte Set-Complex `K` and Subcomplex `Kh` around `<set>` and its complement |
| `set-tool nexus <set1> <set2>` | Find every catalog nexus set class holding a `K` or `Kh` relation with all given sets simultaneously |
| `set-tool comb <hexachord>` | Analyze a 6-note hexachord for `P`/`R` and `I`/`RI` aggregate combinatoriality |
| `set-tool cycle <k \| k1,k2> [start]` | Generate a basic `Ck` cycle (with all 12-tone cosets) or an alternating `Alt(k1,k2)` cycle |
| `set-tool cycle-fit <set>` | Find all basic and alternating interval cycles containing `<set>` (marking `[CONTIGUOUS]` vs `[SUBSET]`) |

### Polytonality & Anti-Resolution Commands (`Polytonal`)

Layer arguments accept either `Root:mode` syntax (e.g., `B:phrygian`, `F:minor`, `D:lydian`, `B:min`, `F:maj`, `G:dom7`, `C:dim`, `E:aug`, `A:min7`, `Bb:maj7`) or raw pitch-class lists (`11,2,6` or `B,D,F#`).

| Command | Description |
| :--- | :--- |
| `set-tool poly <layerA> <layerB>` | Analyze two superimposed tonal layers for ambiguity score, cross-layer `ic1`/`ic6` friction, single-scale absorption, and accidental `V7` (`4-27`) resolution traps |
| `set-tool poly-step <A1> <B1> <A2> <B2>` | Validate a two-chord bitonal transition against contrary-motion tritone resolutions, leading-tone cadences, parallel root locking, and single-triad/scale collapse |
| `set-tool poly-partners <layerA> [mode]` | Search all 12 transpositions of candidate scales or triads against `<layerA>`, ranked by highest bitonal independence and zero resolution traps |
| `set-tool poly-multi <L1> <L2> <L3> ...` | Analyze 3 or more simultaneous tonal layers for aggregate saturation, universal pivots, pairwise layer breakdown, and global cross-layer `V7` traps |
| `set-tool poly-multi-partners <L1> <L2> [mode]` | Rank the best non-resolving 3rd tonal layer to add on top of an existing two-layer foundation `<L1> + <L2>` |

### Examples

**Analyze a set (including Forte vs. Rahn prime form comparison on diverging sets like `5-20`):**

```bash
cabal run set-tool -- 0,1,4,6
cabal run set-tool -- 5-20
cabal run set-tool -- C,Db,Gb,G,Bb
```

**Find minimal voice-leading distance, exact `Tn`/`TnI` mappings, and Z-mates:**

```bash
cabal run set-tool -- voicelead 0,1,6,7 4-Z15
cabal run set-tool -- 0,1,6,7 2,3,8,9
cabal run set-tool -- 4-Z15 4-Z29
```

**Search supersets, subsets, `K`/`Kh` complexes, nexus sets, and combinatoriality:**

```bash
cabal run set-tool -- supersets 3-5 5
cabal run set-tool -- complex 4-Z15
cabal run set-tool -- nexus 3-5 4-Z15 5-19
cabal run set-tool -- comb 6-7
```

**Generate interval cycles and test cycle alignment:**

```bash
cabal run set-tool -- cycle 3 B
cabal run set-tool -- cycle 1,3 0
cabal run set-tool -- cycle-fit 0,1,6,7
```

**Analyze 2-layer and 3+ layer polytonality, validate steps, and find non-resolving partners:**

```bash
cabal run set-tool -- poly B:phrygian F:minor
cabal run set-tool -- poly-step B:maj F:maj D:min Ab:min
cabal run set-tool -- poly-partners B:min min
cabal run set-tool -- poly-multi B:min F:min D:maj
cabal run set-tool -- poly-multi-partners B:min F:min maj
```

---

## 4. Tuning Reference (`rootOffset` & `baseOctave`)

When using `serial-compose --root` (without `--octave`), octaves are selected automatically for low tunings (`A1`–`B1` for A/Bb/B, `C2`–`Ab2` for C–Ab), with Bass placed one octave lower:

| Tuning | Lowest Pitch | `rootOffset` | `baseOctave` (Guitar) | `baseOctave` (Bass) |
| :--- | :--- | :---: | :---: | :---: |
| **E Standard** | E | `4` | `2` (`E2`) | `1` (`E1`) |
| **Drop D** | D | `2` | `2` (`D2`) | `1` (`D1`) |
| **Drop C** | C | `0` | `2` (`C2`) | `1` (`C1`) |
| **Drop B** | B | `11` | `1` (`B1`) | `0` (`B0`) |
| **Drop A (7-String)** | A | `9` | `1` (`A1`) | `0` (`A0`) |

---

## 5. Using the Libraries in GHCi

Launch an interactive session with `SerialComposer`, `PitchClassSet`, and `Polytonal` loaded:

```bash
cabal repl
```

```haskell
import SerialComposer
import PitchClassSet
import Polytonal
import System.Random (mkStdGen)

-- Analyze 3-layer polytonal independence (e.g., Guitar + Bass + Synth)
let Just l1 = parseLayerSpec "B:min"
let Just l2 = parseLayerSpec "F:min"
let Just l3 = parseLayerSpec "D:maj"
fmap mpOverallAmbiguity (analyzeMultiPoly [l1, l2, l3])

-- Generate a derived 12-tone row, scan 4N invariance, and print its rotational array
let (Just dRow, _) = deriveRow12 (mkStdGen 42)
printRowSegmentation 11 dRow
printRowInvariance 12 11 dRow
```

---

## 6. Compiling & Viewing Scores

### MuseScore (MusicXML)

```bash
mscore doom_b.musicxml
```

### LilyPond (PDF Sheet Music + Audio MIDI)

```bash
lilypond doom_b.ly
# Generates doom_b.pdf and doom_b.midi
```
