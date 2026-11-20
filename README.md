# SerialComposer

`SerialComposer` is a pure Haskell library suite and set of command-line tools (`serial-compose` and `set-tool`) for **N × N serial matrix generation**, **Allen Forte pitch-class set theory analysis**, and **MusicXML / LilyPond score export**.

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
    ├── SerialComposer.hs (N x N matrix engine, groove realization, score export)
    └── PitchClassSet.hs  (Allen Forte set theory catalog, Z-relations, similarity & search)
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
| `-n` | `--length` | `INT` | Number of unique pitches in random row (default: `6`) |
| `-m` | `--mod` | `INT` | Modulus / chromatic division (default: `12`) |
| `-s` | `--seed` | `INT` | Deterministic integer RNG seed (default: random system seed) |
| `-r` | `--root` | `NOTE` | Root note name: `C`, `Db`, `D`, `Eb`, `E`, `F`, `Gb`, `G`, `Ab`, `A`, `Bb`, `B` (default: `C`) |
| `-o` | `--octave` | `INT` | Override base octave for guitar (default: auto based on root) |
| `-p` | `--part` | `MODE` | Instrument mode: `guitar`, `bass`, or `duo` (default: `duo`) |
| | `--plain` | | Use plain quarter notes without rhythm/octave displacement |
| `-a` | `--analyze` | | Print Forte pitch-class set segmentation of P0 (requires `--mod 12`) |
| `-t` | `--tempo` | `BPM` | Tempo in BPM for LilyPond MIDI export (default: `75`) |
| `-f` | `--out` | `PREFIX` | Output filename prefix without extension (default: `serial_output`) |
| `-h` | `--help` | | Show help message |

### Examples

**Generate a random 5-tone Drop B Guitar + Bass counterpoint score with a fixed seed:**

```bash
cabal run serial-compose -- --length 5 --root B --seed 42 --tempo 68 --out doom_b
```

**Analyze the discrete partitions and sliding windows of a 12-tone row using Forte set theory:**

```bash
cabal run serial-compose -- -n 12 -r C -s 42 --analyze
```

**Export a clean, un-displaced solo guitar study sheet in Drop D:**

```bash
cabal run serial-compose -- -w "0,1,6,7,10" -r D -p guitar --plain -f drop_d_study
```

---

## 3. Pitch-Class Set Theory Tool (`set-tool`)

`set-tool` implements Allen Forte's *The Structure of Atonal Music*, including the complete 208-class Forte catalog (`3-1` through `9-12`), Normal Order, Prime Form, Interval-Class Vectors, Z-relations, similarity relations (`R1`, `R2`, `Rp`), and abstract superset/subset searches.

### Commands

| Command | Description |
| :--- | :--- |
| `set-tool <set-or-forte-name>` | Full analysis: Normal Order, Prime Form, Interval Vector, Complement, Z-Mate, `Tn` common tones, `R1`/`R2`/`Rp` similar sets, and embedded trichords |
| `set-tool <set-1> <set-2>` | Compare two sets for `Tn`/`TnI` equivalence, Z-relation, and `R1`/`R2`/`Rp` similarity |
| `set-tool supersets <set> [card]` | Find all Forte catalog supersets containing `<set>` (optionally filtered to target cardinality `card`), with embedding multiplicity `[xN]` |
| `set-tool subsets <set> [card]` | Find all Forte catalog subsets contained in `<set>` (optionally filtered to target cardinality `card`), with embedding multiplicity `[xN]` |

### Examples

**Analyze a set by pitch-class integers, note names, or Forte catalog name:**

```bash
cabal run set-tool -- 0,1,4,6
cabal run set-tool -- C,Db,Gb,G,Bb
cabal run set-tool -- 4-Z15
```

**Compare two sets for Z-relations and interval-vector similarity:**

```bash
cabal run set-tool -- 4-Z15 4-Z29
cabal run set-tool -- 0,1,6 0,1,4
```

**Search the Forte catalog for supersets or subsets:**

```bash
# Find all pentachords containing the Viennese trichord 3-5 (0,1,6)
cabal run set-tool -- supersets 3-5 5

# Find all tetrachord subsets inside 5-19 (0,1,3,6,7)
cabal run set-tool -- subsets 5-19 4
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

Launch an interactive session with both `SerialComposer` and `PitchClassSet` loaded:

```bash
cabal repl
```

```haskell
import SerialComposer
import PitchClassSet
import System.Random (mkStdGen)

-- Analyze a set using PitchClassSet
let s = makeSet [0, 1, 6, 7, 10]
forteName s          -- "5-19"
primeForm s          -- [0,1,3,6,7]
intervalVector s     -- [2,2,1,1,3,1]
findSupersets (Just 6) [0, 1, 6]

-- Generate a matrix and inspect its Forte segmentation
let mat = generateMatrix 12 s
printNoteMatrix 11 mat
printRowSegmentation 11 s
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
