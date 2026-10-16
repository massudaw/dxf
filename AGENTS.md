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

- What to build, in this order (the steps): finished work, long description moved (details: docs/brief-archive.md, section "What to build, in this order (the steps)").

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

- Part three: the low-level code on linbuf: finished work, long description moved (details: docs/brief-archive.md, section "Part three: the low-level code on linbuf").
- Part four: the tables on membudget: finished work, long description moved (details: docs/brief-archive.md, section "Part four: the tables on membudget").
## The libraries' adoption requests (2026-10-15)

The user's word (2026-10-15): adopt what the user's libraries measured for this project. Each request below is a
report you can read whole: `ghci-session reports show ID` (and the library's `docs/adopting.md`, section for this
project; its port is a COPY under that library's `bench/port/`). The libraries are the user's own, READ ONLY for
you; what one lacks or does wrong is a `report` with `about: LIBRARY`. All of them have a tagged v0.1.0; build
against their working trees as now (`cabal.project` lists them), and say the commit you measured against.

- **linbuf** (report `034a407c`): linbuf-lint found a real thunk in Tag.Binary.$wscanBinary (each tag's clamped position built lazily) and 29 stores holding a thunk: fix what the lint says, measured.
- **portkit** (report `2f21c0f8`): move test/Check/{Util,Oracle}.hs onto portkit: ezdxf's verdicts kept on disk by drawing and ezdxf's version (warm full check 104 -> 27 s CPU).
- **linstream** (report `52eee175`): write documents through linbuf's sink (dxf-write-bench peak 2,177 MB, about -358 MB).

How, as before: a baseline first (outputs kept, the figures: instructions, CPU, peak), then ONE request a commit,
in the order above: the SAME OUTPUT on every fixture and sample, time within 3% or better, peak equal or lower
(a streaming move should be clearly lower: say how much). A request that comes out worse is NOT taken: PERF.md
says why, and a report to the library says what it would need. Then mark each report: `ghci-session reports set
ID fixed NOTE` when taken, `... dismissed NOTE` when not, with the figure. Commit and push each; say WAITING
when done.

## What is not committed

Data and compiled files stay out of the repository (`.gitignore`): no `*.dxf` -- the samples are another
project's, and the fixtures in `test/data/` are written by `tools/gen_all.sh` (every `tools/gen_*.py`, run
with the reference's Python) -- and no `__pycache__`, `*.pyc`, objects or profiles. A fixture a test needs
has to come out of a generator that is committed: the `r12`/`r2007`/`r2018` `.txt.dxf` and `.bin.dxf`
fixtures were made by hand from samples with `tools/genbin.py` and have no generator yet -- give them one.

## Faster iteration (the user's word, 2026-10-15: "checks are getting too expensive, too many guardrails; iteration on changes should be much faster") -- this OVERRIDES any rule above about measuring and checking

- **While working: the narrowest check that covers the change.** After a save, run the check group (or the one test)
  for what you changed -- `test` with a scoped expression, portkit's selection where the project has it -- not the
  whole check. Seconds, not a minute.
- **The whole check once, before a commit** (or a few commits batched), not after every save.
- **Measure only when the change can move speed or memory**: aimed at performance, or touching a hot path (a
  parser's inner loop, a writer, the solver's step). A rename, a doc, a check, a refactor that compiles to the same
  code, an error message: compile + scoped check + the same outputs is enough -- no benchmark.
- **When you do measure**: instructions are the steady figure -- 1 to 3 runs, not 5 to 9; CPU and wall only when
  instructions cannot say it. The 3% rule applies to performance changes, not to every change.
- **Releases in batches**: tag when a few changes have gathered or someone is waiting for one, not after each. The
  adopters' check only on adopters whose dependency changed; a canary only at a minor version.
- **Kept**: the same outputs (portkit-same / the oracle) before a commit, the disk and memory floors, never loading
  a half-built library -- they cost milliseconds and caught real damage.

## Releases batched (the user's word, 2026-10-16, from agent-des's fleet model: a release every change costs the fleet
## 19% of its commits; releases twice as rare give +6%) -- this sharpens "Faster iteration"'s releases rule

- **Tag only when someone is waiting for it** (a report or the overseer asks for a version, a user needs a fix) **or
  when the unreleased changes are worth a release on their own** -- not after each change. Several fixes and features
  go into one tag.
- **One release run per batch**: the adopters' check and any canary once for the batch, not once per commit.
  Between releases, commit and push as usual; adopters that pin a commit (`source-repository-package` / `../X-stable`)
  can take a commit without a tag when they need it.
- **Say what is unreleased** in your WAITING line (e.g. "3 changes since v0.1.4, none asked for") so the overseer can
  call a release when one is due.
