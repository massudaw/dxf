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
