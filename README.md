# SerialComposer

`SerialComposer` is a pure Haskell library and command-line tool (`serial-compose`) for generating **N × N serial matrices** (12-tone or smaller pitch-class sets), applying **rhythm and octave-displacement transformations**, and exporting scores to **MusicXML** (MuseScore) and **LilyPond** (PDF & MIDI).

---

## Project Structure

```text
serial-composer/
├── serial-composer.cabal
├── README.md
├── app/
│   └── Main.hs
└── src/
    └── SerialComposer.hs
```

---

## Requirements

- **GHC** (9.8+ compatible, zero `-Wall` or `-Wx-partial` warnings)
- **Cabal** (`cabal-install` 3.0+)
- **Optional:** [MuseScore](https://musescore.org) for opening `.musicxml` files, or [LilyPond](https://lilypond.org) for compiling `.ly` files to PDF and MIDI.

---

## 1. Building & Installing

Build the library and CLI executable:

```bash
cabal build
```

Optional: Install `serial-compose` to your local Cabal bin directory (`~/.cabal/bin` or `~/.local/bin`):

```bash
cabal install
```

---

## 2. CLI Usage (`serial-compose`)

Run directly via `cabal run serial-compose -- [OPTIONS...]` or using the installed `serial-compose` binary.

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
| `-t` | `--tempo` | `BPM` | Tempo in BPM for LilyPond MIDI export (default: `75`) |
| `-f` | `--out` | `PREFIX` | Output filename prefix without extension (default: `serial_output`) |
| `-h` | `--help` | | Show help message |

### CLI Examples

**Generate a random 5-tone Drop B Guitar + Bass counterpoint score with a fixed seed:**
```bash
cabal run serial-compose -- --length 5 --root B --seed 42 --tempo 68 --out doom_b
```

**Use a custom note-name row in Drop C (Duo mode):**
```bash
cabal run serial-compose -- --row "C,Db,E,Gb,G,Bb" --root C --part duo --out drop_c_hex
```

**Use a custom integer pitch-class row in Drop B:**
```bash
cabal run serial-compose -- -w 0,1,6,7,10 -r B -s 99 -f custom_doom_b
```

**Export a clean, un-displaced solo guitar study sheet in Drop D:**
```bash
cabal run serial-compose -- -w "0,1,6,7,10" -r D -p guitar --plain -f drop_d_study
```

---

## 3. Tuning Reference (`rootOffset` & `baseOctave`)

When using the CLI `--root` flag (without `--octave`), octaves are selected automatically for low tunings (`A1`–`B1` for A/Bb/B, `C2`–`Ab2` for C–Ab), with Bass placed one octave lower. For direct GHCi library calls, use these reference values:

| Tuning | Lowest Pitch | `rootOffset` | `baseOctave` (Guitar) | `baseOctave` (Bass) |
| :--- | :--- | :---: | :---: | :---: |
| **E Standard** | E | `4` | `2` (`E2`) | `1` (`E1`) |
| **Drop D** | D | `2` | `2` (`D2`) | `1` (`D1`) |
| **Drop C** | C | `0` | `2` (`C2`) | `1` (`C1`) |
| **Drop B** | B | `11` | `1` (`B1`) | `0` (`B0`) |
| **Drop A (7-String)** | A | `9` | `1` (`A1`) | `0` (`A0`) |

---

## 4. Using `SerialComposer` as a Library in GHCi

Launch an interactive session with the library loaded:

```bash
cabal repl
```

### Generate & Inspect a Matrix

```haskell
import SerialComposer
import System.Random (mkStdGen)

-- 1. Generate a random 6-tone row in chromatic space (mod 12)
let (randRow, g1) = randomRow 12 6 (mkStdGen 42)

-- Or define a custom 5-tone row manually:
let doomRow = [0, 1, 6, 7, 10]

-- 2. Build the N x N matrix
let mat = generateMatrix 12 doomRow

-- 3. Print raw pitch-class integers (0-11)
printMatrix mat

-- 4. Print transposed note names (e.g., root = 11 for Drop B)
printNoteMatrix 11 mat
```

### Extract Row Transformations

```haskell
let p0  = getPrime mat 0               -- First row (left -> right)
let r0  = getRetrograde mat 0          -- First row (right -> left)
let i0  = getInversion mat 0           -- First column (top -> bottom)
let ri2 = getRetrogradeInversion mat 2 -- Third column (bottom -> top)
```

### Export Custom Multi-Part Scores

```haskell
let gtrRows  = [("Section A", getPrime mat 0), ("Section B", getInversion mat 0)]
let bassRows = [("Section A", getRetrograde mat 2), ("Section B", getRetrogradeInversion mat 3)]

let (gtrSecs, genAfterGtr) = realizeSectionsGroove 11 1 2 25 (mkStdGen 77) gtrRows
let (bassSecs, _)          = realizeSectionsGroove 11 0 1 10 genAfterGtr bassRows

let fullScore =
      [ PartSpec "Distorted Guitar" "guitar" "distorted guitar" gtrSecs
      , PartSpec "Distorted Bass"   "bass"   "electric bass (pick)" bassSecs
      ]

exportMusicXMLParts "doom_counterpoint.musicxml" fullScore
exportLilyPondParts "doom_counterpoint.ly" 68 fullScore
```

---

## 5. Compiling & Viewing Scores

### MuseScore (MusicXML)
```bash
mscore doom_b.musicxml
```

### LilyPond (PDF Sheet Music + Audio MIDI)
```bash
lilypond doom_b.ly
# Generates doom_b.pdf and doom_b.midi
```
