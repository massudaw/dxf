# This project: a DXF library in Haskell

The goal is a complete DXF parser and printer, modelled on ezdxf, the main Python library for the format.

## What is here

- `src/` -- the library as it was: a partial parser (attoparsec) and writer, with types for a few entities.
  It is a starting point, not a design to keep: replace what stands in the way.
- `.ref/venv/` -- a Python environment with ezdxf installed. Its source is the reference for what the format
  holds and how it is laid out: `.ref/venv/lib/python3.14/site-packages/ezdxf/` (see `lldxf/` for the tag
  layer -- tagger, tags, loader, validator, tagwriter -- `sections/` for the file's structure, `entities/` for
  every entity and its group codes). `.ref/venv/bin/python` runs it, which makes it an oracle: load a file
  with ezdxf, count and dump what it finds, and compare with what this library finds.
- `samples/` -- 93 real DXF files (42 MB together; from 3 KB to 19 MB, `samples/f001.dxf` the largest), links
  to another project's files. Read them, never write to them. They are not committed.

## What to build

1. A tag layer: a DXF file is a sequence of (group code, value) pairs. Read it exactly and write it back
   exactly. Both text encodings a file may be in, both line endings, and the binary DXF format too.
2. The structure over it: HEADER (every variable), CLASSES, TABLES (LAYER, LTYPE, STYLE, DIMSTYLE, VPORT,
   VIEW, UCS, APPID, BLOCK_RECORD), BLOCKS, ENTITIES, OBJECTS, THUMBNAILIMAGE.
3. Typed entities, as far as ezdxf has them: LINE, POINT, CIRCLE, ARC, ELLIPSE, LWPOLYLINE, POLYLINE with
   its VERTEX and SEQEND, SPLINE, TEXT, MTEXT, INSERT with its ATTRIB, ATTDEF, DIMENSION, LEADER, HATCH,
   SOLID, TRACE, 3DFACE, VIEWPORT, IMAGE, and the rest. An entity or an object the library has no type for is
   kept as its tags, so that nothing is ever lost: a file read and written back holds everything it held.
4. A printer that writes what was read: `print . parse` gives a file that parses to the same value, and for
   a file that was already in the form this library writes, the same bytes.

## How it has to be

- **Efficient operations on `ByteString` or `Text`, never `String`.** The input is read as a strict
  `ByteString`; a tag's value is a slice of it, not a copy, until something needs it decoded. Numbers are
  parsed from the bytes directly. Output is built with a `Builder`. No `String`, `read`, `show` or `++` on
  the path a file's bytes take.
- **Measured, not assumed.** Profile memory and CPU on the samples -- the largest first -- and work on what
  the measurements show. The session's tools do this warm: `bench` times an IO action (wall, GC,
  allocation), `census` says what the heap holds, `mem` the memory of the repl. For a cost-centre or a heap
  profile, build a small executable with profiling and run it with `sh` (`+RTS -p`, `-hT`, `-s`). Keep the
  numbers: a table in `PERF.md` of each large sample -- its size, time and peak memory to parse, to print,
  and the same for ezdxf -- updated as the work changes them. Aim to parse at well over 100 MB a second and
  to hold a parsed file in a small multiple of its size.
- **Checked against the samples.** Every sample parses; every sample survives the round trip; what was
  found in each (entities by type, layers, blocks) is what ezdxf finds. A test suite in the cabal file does
  this, and the session's `test` runs it (set `"test"` in `ghci-session.json` once it exists). The session
  takes a change to `ghci-session.json` by itself: a second after it is saved, the repl is restarted on it.
- Modules stay under 250 lines each: one per concern (tags, each section, groups of entities).

## How to work

The session loads the library COMPILED and optimised (`"optimize": true` in `ghci-session.json`): an
`eval` or a `bench` on the largest sample runs at the speed of the built library, so what the session
measures is real. What is typed into `eval` itself is still interpreted: keep loops out of the expression
and inside functions of the library (a small `DXF.Debug` module of measuring helpers is fine).

The code is loaded in a warm GHCi session: `eval` answers in milliseconds, a file written is reloaded and
its verdict comes back with the write. Use that rather than building by hand. Commit as the work goes, in
steps that each build and pass, on the branch `full-parser`. Do not push.

Work in one loop: do not use `spawn` or `tell`.

For the routine check, use the session's `test` tool (the suite is wired to it, and the session has the code
compiled already): `cabal build` of the test executable compiles everything a second time, twenty seconds a
run. Keep the built executables for what needs them -- the profiled runs and the numbers in PERF.md.

## Part three: the low-level code on linbuf

The user's word (2026-10-14): dxf moves its low-level byte and bit code onto **linbuf** (`~/code/linbuf`: the
user's own package -- linear-typed buffers: a buffer has one owner and is frozen without a copy; a reserved length
or CRC is a slot that must be filled exactly once; reads checked once for n bytes; base, bytestring and ghc-prim
only; the user has allowed this dependency). linbuf's agent studied this project and wrote what would move and
what it measured: `~/code/linbuf/docs/adopting.md`, "## dxf", and `PERF.md`. linbuf is READ ONLY for you:
what it lacks or does wrong is a `report` with `about: linbuf` (it reaches its agent through the user), never an
edit there. dwg is doing the same move now: its `AGENTS.md` Part three and `PERF.md` may show what worked.

`cabal.project` now has `../linbuf`; add `linbuf` to the library's `build-depends`.

In this order, each a commit that builds and passes the whole check, pushed:

1. **The baseline.** Before any change: `bench exe` with a named `baseline` on the programs and samples named
   below -- CPU, instructions, allocation, peak -- and their outputs kept under `.ghci-session/linbuf-base/` (one
   a sample; the disk is small). Every later step is held against both: the SAME BYTES, and within 3% or better.
   Programs: dxf's own bench (`dxf-write-bench`, parse and print of the samples in its PERF.md).
2. **`DXF.Tag.Prim`** (12 `unsafeIndex`) on `LinBuf.Read` / `LinBuf.Index`.
3. **`DXF.Intern`** (`unsafeRead`/`unsafeWrite` on a hash) on `LinBuf.Ints` (`readMasked`).
4. **`DXF.Print`**'s BoundedPrims onto `LinBuf.Write` (`putBounded`, `decimal`) -- only if it costs nothing: the
   printer is already fast (PERF.md). Doubles (`DXF.Tag.Double`) are not covered: leave them.
   This move is for SAFETY alone (linbuf ranked dxf last): stop at the first step that costs, and say so.
- **What is left**: every `unsafe*`, `Ptr`, `MagicHash` still here, each with why it stays (linbuf lacks it:
  report it; or it is right as it is); `PERF.md`: the table of every step against the baseline.

A module that comes out slower is NOT moved: say why in `PERF.md`, report what linbuf would need, leave it.
Say WAITING when these are done, or when a step cannot be done without a change in linbuf (reported).

## What is not committed

Data and compiled files stay out of the repository (`.gitignore`): no `*.dxf` -- the samples are another
project's, and the fixtures in `test/data/` are written by `tools/gen_all.sh` (every `tools/gen_*.py`, run
with the reference's Python) -- and no `__pycache__`, `*.pyc`, objects or profiles. A fixture a test needs
has to come out of a generator that is committed: the `r12`/`r2007`/`r2018` `.txt.dxf` and `.bin.dxf`
fixtures were made by hand from samples with `tools/genbin.py` and have no generator yet -- give them one.
