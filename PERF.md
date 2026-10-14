# Performance

Apple M-series laptop, GHC 9.14.1 `-O2`, single thread.  Library side: `dxf-bench FILE [doc] +RTS -s`
(wall time of each stage; "doc" skips the tag-layer stages; max residency is GHC's live data, peak RSS
from `/usr/bin/time -l`).  ezdxf 1.4.4 on Python 3.14: `tools/perf_ezdxf.py` (peak RSS includes the
~55 MB of the interpreter and its imports).

## Whole document: parse to typed entities, print back

"parse" is bytes to a fully evaluated `Document` (scan, tables, blocks, typed entities).  "print" is
`Document` to bytes.  Both outputs are checked: the printed file parses to the same `Document`, and
every sample and fixture is compared with ezdxf (`Check.run`).  Best of three runs.

The ten largest samples (`samples/3CG` is a second copy of f001 and is left out; `*` marks the two small
files this round works on).  Measured with the executable (`dxf-bench FILE table`, a fresh process per file):

* **cold** is the first parse (or print) in the process, code not yet warm; **loop** is the best of N repeats
  in the same process (N = 7 for f001, 57-300 for the rest; each repeat parses a fresh copy of the input,
  GC included) -- the same figures come out of `bench` with `opt: 2`, `unit: exe:dxf-bench` and the args
  `["FILE","table"]` (the session's process is warm before the first call, so only its loop figures mean something);
* **max residency** is GHC's "maximum residency" of `dxf-bench FILE parse +RTS -s` (the parsed document, the
  input bytes and the printed output of one run);
* **ezdxf** is `tools/perf_ezdxf.py` (`ezdxf.readfile`, `doc.write` to a `StringIO`), peak RSS from
  `/usr/bin/time -l` (it includes ~55 MB of interpreter and imports).

| file | size | parse cold ms | MB/s | parse loop ms | MB/s | print cold ms | MB/s | print loop ms | MB/s | max residency | ezdxf read ms | MB/s | ezdxf write ms | MB/s | ezdxf peak RSS |
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| f001.dxf (112,322 LINE) | 19.35 MB | 146.9 | 132 | 130.8 | 148 | 109.8 | 176 | 90.6 | 214 | 44.20 MB | 3279 | 5.9 | 3538 | 5.5 | 263 MB |
| CPQ-SPS-ZZ-ZZ-DR-A-0005 * | 0.51 MB | 12.2 | 42 | 9.8 | 52 | 7.0 | 73 | 3.1 | 164 | 3.87 MB | 275 | 1.9 | 298 | 1.7 | 64 MB |
| condo_A-301_arch-elevation * | 0.47 MB | 10.3 | 46 | 9.0 | 52 | 6.0 | 78 | 2.9 | 162 | 3.59 MB | 227 | 2.1 | 278 | 1.7 | 62 MB |
| CPQ-SPS-ZZ-ZZ-DR-S-0003 | 0.33 MB | 3.4 | 98 | 2.2 | 151 | 7.4 | 45 | 4.8 | 69 | 1.75 MB | 106 | 3.1 | 113 | 2.9 | 57 MB |
| CPQ-SPS-ZZ-ZZ-DR-A-0006 | 0.18 MB | 3.4 | 52 | 2.8 | 64 | 3.7 | 48 | 1.3 | 137 | 1.43 MB | 90 | 2.0 | 104 | 1.7 | 56 MB |
| condo_S-103_frame | 0.17 MB | 1.6 | 105 | 1.1 | 152 | 5.0 | 34 | 2.4 | 70 | 0.85 MB | 59 | 2.8 | 58 | 2.9 | 54 MB |
| sao_jorge_FP-101_plan | 0.16 MB | 3.0 | 52 | 2.0 | 78 | 3.2 | 49 | 1.0 | 157 | 1.19 MB | 82 | 1.9 | 78 | 2.0 | 56 MB |
| sao_jorge_FP-102_riser | 0.15 MB | 3.2 | 48 | 2.0 | 77 | 3.3 | 47 | 1.1 | 141 | 1.18 MB | 83 | 1.9 | 79 | 2.0 | 57 MB |
| condo_A-201_arch-section | 0.14 MB | 2.9 | 50 | 2.1 | 69 | 3.3 | 44 | 1.1 | 131 | 1.19 MB | 75 | 1.9 | 83 | 1.7 | 56 MB |
| CPQ-SPS-ZZ-ZZ-DR-E-0001 | 0.13 MB | 1.4 | 94 | 0.9 | 146 | 3.5 | 38 | 0.9 | 146 | 0.31 MB | 58 | 2.3 | 45 | 2.9 | 55 MB |

The machine was lightly loaded (load average 3.7); loop figures move by 10-20 % between runs.  f001 is parsed
22 times and printed 32 times faster than ezdxf (cold), the two small files 22 and 45 times faster, and a
parsed f001 is held in 2.3 times its size.  Parse is above 100 MB/s on f001 (132 cold), on S-0003, S-103 and
E-0001 (mostly LINE and a few entities, 94-105 cold); the files made of POLYLINE/VERTEX (the two `*`) or TEXT-heavy
drawings are at 42-52 MB/s: they are the target of the allocation work below.  Print of the files that
use entities without a fast writer (S-0003, S-103, E-0001: TEXT, MTEXT, LWPOLYLINE) is slower than their parse.

Before the optimisation round (commit 54a1d27): f001 parse 410 ms (47 MB/s), print 720 ms, residency 100 MB,
RSS 227 MB; CPQ 18/32 ms, condo 17/31 ms.

### What each step bought (f001, `dxf-bench FILE parse`, ms, best of three)

| step | commit | parse | print | residency |
|------|--------|------:|------:|----------:|
| start of the round | 54a1d27 | 468 | 704 | 100 MB |
| LINE decoded straight from the bytes (`Entity.Fast`), layer/owner/linetype shared, V3 fields unpacked, strict `readDouble` | 078bcc4 | 257 | 689 | 90 MB |
| Grisu2 double formatter writing into the buffer (`Tag.Double`) | e9bc1cb | 260 | 275 | 93 MB |
| Eisel-Lemire for 17-digit doubles, group-code line read in one pass (`Tag.Text`) | 1e59861 | 200 | 240 | 41 MB |
| lazy power table, strict Grisu setup, no Integer on the fast path, one primitive per text tag (`Tag.Prim`), LINE without the field table | e327ed2 | 138 | 217 | 41 MB |
| a whole LINE (head and numbers) as one primitive (`Entity.FastOut`), plain head as one primitive | 3ce585d, b61b6b7 | 138 | 85 | 43 MB |

The tag layer alone: scan to `[Tag]` 103 ms (streamed and dropped, 32 ms), print back 88 ms.

## Where the time goes now

Profile of f001 (profiled build, `-fprof-auto-top`; inflated, only the order matters), before the last
two steps: `commonHead` 19 % time / 29 % alloc, Grisu digits 13 %, Integer additions in `readDouble`
10 % (removed since: the mantissa loop kept its counter as an Integer), `bytesTag` 8 %, `lineOut` 8 %.
Heap profile (`-hT`) of the parsed f001 before the residency work: `Common` 22.8 MB, `Line` 15.8 MB,
`Entity` 8.8 MB, `Maybe` 7 MB; the 38 MB of ARR_WORDS is the input.  The shared layer/owner strings
and unpacked points took residency from 100 to 41 MB.

Small POLYLINE/TEXT files (CPQ-SPS...0005): print is dominated by `encodeWith` and `commonHead`
(field-table folds allocate a closure per field), parse by the general decoder.

## The small files: where the 14 ms go (profile, before the work on them)

Both small samples are AC1009 drawings of POLYLINE / VERTEX / SEQEND (`tools/census.py`: VERTEX 69 % of
the bytes, POLYLINE 20 %, SEQEND 6 %, LINE and TEXT 3 %; about 9,800 entities in 500 KB, 53 bytes per
VERTEX).  `dxf-bench FILE stages` times the scan and then each section (forced whole, in turn):

| stage (CPQ-SPS-ZZ-ZZ-DR-A-0005, ms) | time |
|-------|-----:|
| scan tags alone (streamed, dropped) | 1.3 |
| HEADER | 0.0 |
| TABLES | 0.3 |
| BLOCKS | 0.3 |
| ENTITIES | 13.5 |

So the header, tables and blocks are a fixed cost of under 1 ms and the tag scan is 10 %: the time is the
general entity path, 1.4 us per entity.  Allocation says the same: parsing the file allocates 71 MB (140
bytes per input byte; f001, which is all LINE, 14 bytes per input byte).  Profile (`loop`, 30 parses,
`-fprof-auto-top`, inflated; shares of time): the codec machinery `fFin`/`decodeWith`/`codec`/`lens`/`pt3`
34 %, `splitTags` and `break` (cutting each entity's tags into common and own) 13 %, `scanFrom` building the
tag list 9 %, `rnf` forcing the result 10 %, `decodeEntity`/`entityCodec`/`lift` 8 %, `shareCommon` 3 %.
Fast decoders for the entities that fill these files (VERTEX, POLYLINE, SEQEND, TEXT, INSERT...) skip all of it.

## String interning (one table per parse)

`DXF.Intern`: a direct-mapped cache of 1024 slots in front of a `Map` (strings up to 64 bytes, at most 20,000
of them); a hit is a hash of the bytes and one comparison, a miss copies the string once (`B.copy`, so the
table never keeps a slice of the input alive).  One table is made per parse and used by the fast decoders
(`commonR` interns layer, linetype, owner; `strI`/`mstrI` the block name of an INSERT and the style of a TEXT)
and by the general path (`DXF.Entity.Interned`: type name, layer, linetype, owner, application names of
extended data, text/dimension styles, INSERT block name, attribute tags, a layer's linetype, class names,
the tables' entries).  Handles and text are unique and are not interned.
`dxf-bench FILE census` counts what a parsed document holds (values held / distinct by value / distinct in memory):

| field | file | held | distinct values | in memory before | in memory after |
|-------|------|-----:|----:|----:|----:|
| layer | f001 | 112,324 | 47 | 185 | 47 |
| owner | f001 | 112,324 | 2 | 3 | 2 |
| type | f001 | 112,324 | 2 | 3 | 2 |
| layer | CPQ-SPS-ZZ-ZZ-DR-A-0005 | 1,841 | 12 | 352 | 12 |
| type | CPQ-SPS-ZZ-ZZ-DR-A-0005 | 1,841 | 6 | 1,739 | 6 |

Before, only a string equal to the previous entity's was shared (`shareCommon`): 185 copies of 47 layers on
f001 as the layers alternate, and every entity type of the general path a slice of the input (1,739 for 6
names).  Max residency and peak RSS (`parse`, machine under load: compare rows of one run only):

| file | residency before | after | RSS before | after |
|------|-----:|-----:|-----:|-----:|
| f001 | 43.5 MB | 44.2 MB | 83.1 MB | 85.2 MB |
| CPQ-SPS-ZZ-ZZ-DR-A-0005 | 2.8 MB | 3.9 MB | 20.5 MB | 21.6 MB |

Interning does not lower residency here: the benchmark holds the input bytes anyway, and the strings were
already few (a slice is 4 words, the same as the pointer to a shared copy); it is correctness of sharing
(a document that outlives its input holds no slice of it) and a pointer-equal comparison.  The residency
rise on the small file is the fast decoders' entities (see below), not the table.  Parse time on f001:
192 ms against 196 ms (no cost measurable).

## Fast decoders for the usual entities

`DXF.Entity.FastKit` is a small reader monad over a byte offset (`dbl`, `mdbl`, `intD`, `pt`, `mpt`, `strI`,
`marks`, `commonR`); `DXF.Entity.Fast` uses it for VERTEX, LINE, TEXT, POLYLINE, SEQEND, INSERT, CIRCLE, ARC,
POINT and LWPOLYLINE: the usual order of tags, else the entity goes the general way.  The suite compares
them with the general decoder on every sample and fixture ("fast decoders read what the general one reads").

| file (best of five, ms) | parse before | parse after | print |
|------|----:|----:|----:|
| f001 | 196 | 192 | 123 / 100 |
| CPQ-SPS-ZZ-ZZ-DR-A-0005 | 15.9 | 11.5 | 22.8 |
| condo_A-301 | 15.5 | 11.4 | 20.9 |

(The machine ran slower than when the earlier tables were made; all rows here are one session.)
The small files still print slower than they parse: next, fast writers.

## Fast writers for the usual entities

`DXF.Entity.FastOut.fastOut` writes LINE, VERTEX, POLYLINE, SEQEND and INSERT as one primitive each (the head
by `headWriter`, the fields by `putDoubleTag`/`putIntTag`/`putBytesTag`), when the head is plain, there is no
extended data, strings are short and numbers plain decimal; children (a POLYLINE's vertices) follow through
`encodeEntity`.  Anything else takes the codec path (`encodeGeneral`).  The suite writes every entity of every
sample and fixture (blocks too, children included) both ways in all eight layouts and compares the bytes.

| file (best of five, ms; the machine was loaded, compare within a row) | print before | print after |
|------|----:|----:|
| CPQ-SPS-ZZ-ZZ-DR-A-0005 | 23.0 | 6.9 |
| condo_A-301 | 22.6 | 6.6 |
| f001 (interleaved best of ten) | 102.6 | 100.3 |

The small files now print faster than they parse (12 ms).

## Steady state, and what the numbers hide (measured with `bench opt:2`)

* A single run of a 500 KB file is 11-12 ms, but that includes cold code; 400 parses in a loop cost
  6.9 ms of mutator each (74 MB/s) plus GC: 4 ms with the default nursery, 1.2 ms with `-A16m`
  (CPQ-0005 and condo_A-301 alike; 20 MB allocated per parse of 0.5 MB). With the whole file in one
  parse the nursery matters little: f001 parses in 131 ms with `-A64m`, 190 ms with the default.
* f001, parse 146 ms (132 MB/s), print 102 ms, in the session at -O2 and as the executable alike.
* The printed f001 parses in 127 ms in the executable, as fast as the original: the 172 ms noted
  earlier and the 660 ms seen once inside the session were GC of the first document being held,
  not a slower path (long digit runs do not fall off the fast path).

## Allocation round: small files, bytes allocated per parse

`dxf-bench FILE loop +RTS -s` (400 parses, allocated bytes / 400, input copy of 0.5 MB included) and
`dxf-bench FILE table` (best of two processes; cold = first parse, loop = best repeat).  Aim: under 10 bytes
per input byte (5 MB) and over 100 MB/s cold.  CPQ = CPQ-SPS-ZZ-ZZ-DR-A-0005 (509 KB), condo = condo_A-301 (470 KB).

| step | commit | CPQ alloc | condo alloc | CPQ parse cold / loop | condo parse cold / loop | f001 parse cold / loop |
|------|--------|----------:|------------:|----------------------:|------------------------:|-----------------------:|
| start (profile showed 1,690 entities per parse on the general path) | 0ca0652 | 20.1 MB (40 B/B) | 18.6 MB | 12.2 / 9.8 ms | 10.3 / 9.0 ms | 147 / 131 ms |
| POLYLINE flags before the point (R12 order) read by the fast decoder; entity loop keeps to the bytes between fast entities (no `Tag` built) | a2a752f | 11.4 MB (22 B/B) | 10.5 MB | 8.7 / 7.0 ms | 8.2 / 6.6 ms | 138 / 130 ms |
| INLINE on the FastKit readers; `readDoubleU` returns an unboxed sum, scanner as top-level functions (no per-number closure) | (this) | 9.2 MB (18 B/B) | 8.4 MB | 8.3 best / 9.1 median ms (bench opt 2, 30 runs, warm) | n/m | not re-measured |

The R12 files write POLYLINE as `66, 70, 10`, the reader wanted `66, 10, 70` and gave up, so all 1,654
polylines of the file (and the 6,500 vertices were fine) went through the tag list and the field-table
codecs: 40 % of the allocation.

## Writing a new drawing (DXF.Build)

`dxf-write-bench N [R12|R2010] [FILE]` builds N LINEs on one layer, each with extended data (an id string,
an int, a real: the shape of an export where every object carries the id of its model element), makes the
document (handles, owners, tables, blocks, extents) and prints it. `-O2`, one run, 100,000 lines:

| version | file | build drawing | to document | print | total | allocated | max residency |
|---|---|---|---|---|---|---|---|
| R2010 | 17.5 MB | 147 ms | 18 ms | 344 ms | 0.53 s | 1.05 GB | 95 MB |
| R12 | 13.0 MB | 180 ms | 57 ms | 252 ms | 0.52 s | 0.99 GB | 84 MB |

The same drawing in ezdxf (`add_line` + `set_xdata`, then `saveas`, R2010): 2.2 s to build, 4.6 s to save,
6.8 s in all, about 13 times slower. Reading the written file back: ezdxf 3.7 s and 318 MB, audit finds
nothing (errors 0, fixes 0), 100,000 lines. About 40 % of the time is the collector (`-A64m` does not
change it: the document is live while it is printed).

Printing is 51 MB/s here against 150+ MB/s for a parsed file: an entity with extended data is not one of
the fast-writer shapes and goes through the field-table codec. A fast writer with XDATA is the next step if
exports of millions of lines matter.

## What is left

* The small files parse at 46-74 MB/s (cold single run / steady state), below the 100 MB/s aim; f001
  reaches it. What remains is allocation (40 bytes per input byte) in the FastKit reader monad and the
  per-entity records; unboxed-sum return types or a fixed-shape VERTEX decoder are the next step.
* Doubles are formatted again (shortest digits: linbuf's Schubfach since the reals' move) rather than
  copied from the source bytes: a copy would cost 20+ bytes per number held in memory.


## The move onto linbuf (AGENTS.md, Part three)

Each step is held against the baseline taken before any change (`bench` with `exe`, baseline `linbuf-base`,
9 runs each, the run of least CPU).  The outputs of the baseline are kept in `.ghci-session/linbuf-base/`
(not committed); every step must write the SAME BYTES and stay within 3 %.  The machine is shared (load
average 9-16 while these were taken): CPU moves by 5-40 % between identical runs of the same binary, so
instructions retired (stable to 0.1 %) are the figure a step is judged by, CPU the best of 9 beside it.

`f001.dxf` in `samples/` is now a broken link; its copy `3CG BASE PLANS (from the SketchUp model).dxf` (the
same 19,345,818 bytes) is linked as `.ghci-session/f001.dxf` (the bench tool splits arguments at spaces).

Programs: `dxf-bench FILE round N OUT` (N parses of a fresh copy, each forced whole, then N prints; the
last print written to OUT), `dxf-bench FILE printloop` (30 prints of one parse), `dxf-write-bench 100000
R2010|R12 OUT` (build, to document, print, write).

| step | commit | f001 round 3: CPU s / G instr | CPQ-0005 round 60 | condo A-301 round 60 | f001 printloop | write 100k R2010 | write 100k R12 | bytes |
|---|---|--:|--:|--:|--:|--:|--:|---|
| baseline | ed43487 | 1.17 / 8.32 | 1.06 / 7.98 | 0.71 / 7.83 | 3.67 / 29.36 | 0.92 / 5.39 | 0.82 / 4.88 | kept |
| 2. tag reader on `LinBuf.Index` | c564e67 | 1.23 / 8.46 (+1.7 %) | 1.22 / 7.92 (-0.8 %) | 1.20 / 7.77 (-0.8 %) | -- | -- | -- | SAME |
| 3. interning on `LinBuf.Ints` | b80e201 | 1.18 / 8.47 (+1.8 %) | 0.96 / 8.11 (+1.6 %) | 0.89 / 7.84 (+0.1 %) | -- | -- | -- | SAME |
| binary reader on `LinBuf.Index` | 2ac2a7c | (see below: +1.2 % on a binary f001) | | | | | | SAME |
| all of it, the six programs | (this) | 1.31 / 8.54 (+2.6 %) | 1.13 / 8.12 (+1.8 %) | 1.07 / 7.83 (0.0 %) | 4.63 / 29.47 (+0.4 %) | 1.09 / 5.42 (+0.6 %) | 0.96 / 4.91 (+0.6 %) | SAME |

Peak and allocation of the baseline: f001 round 179 MB / 854 MB, CPQ 28 / 1034, condo 24 / 957, printloop
83 / 4165, write R2010 233 / 1004, R12 210 / 947.

Step 2, the tag reader.  AGENTS.md (after linbuf's `docs/adopting.md`) names it `DXF.Tag.Prim`, "12
`unsafeIndex`"; those twelve are in `DXF.Tag.Text` (`nextTag`, `scanText`) -- `DXF.Tag.Prim` is the
writer's primitives (step 4).  `DXF.Tag.Text` now reads through `LinBuf.Index`: a group code line is read
from ONE checked 8-byte load (`index64le`) by a state machine kept in one `Int` (a line longer than 8
bytes, or one in the last 8 bytes of the file, byte by byte through `index8`); the value line's end is
found by `B.elemIndex` on `B.drop` (checked).  Tried and dropped on the way: `index8` for every byte as
the old loop read them, +3.0 % instructions on f001 (each read checked again after the loop's own `j < n`);
and `B.take`/`B.drop` for `slice` as well, +5.5 % instructions and +2 % allocation (`B.take` may answer
the shared `empty`, so the slice is no longer built in place).  So `slice` keeps `unsafeTake`/`unsafeDrop`:
its offsets are those `nextTag` found in the same bytes.  linbuf has no checked slice at a position outside
its `Parser` (reported).  CPU on the small files moves 0.7-1.2 s between identical runs on this machine;
instructions are within 2 % on all three.

Step 3, `DXF.Intern`.  The direct-mapped cache was an `IOArray` of the strings themselves, read and written
with `unsafeRead`/`unsafeWrite`.  Now it is a `LinBuf.Ints` of 1024 string NUMBERS (-1 for none), read and
written with `readMasked`/`writeMasked` (the mask is the table's: no check, never out of range); the
strings are held by number in an `IOArray` read with the checked `readArray`, and the map gives the number.
The hash is `B.foldl'` (an `index8` loop cost 1 % more on the small files).  Against step 2 the small CPQ
file pays +2.4 % instructions (the extra read of the string by its number), f001 +0.1 %; against the
baseline all three are within 2 %.  `unsafeDupablePerformIO` stays: `intern` is pure (interning does not
change a value) over a table made for one parse.

Beyond the list: the binary reader, `DXF.Tag.Binary.scanBinary`, read its bytes with `unsafeIndex` and its
strings and binary chunks with `unsafeTake`/`unsafeDrop`; it now reads through `LinBuf.Index.index8` and
`B.take`/`B.drop`.  No sample is binary, so it is measured on f001 written as binary DXF (12.9 MB, made
with `tagsFile` and a binary `Fmt`, under `/tmp`): `dxf-bench /tmp/f001.bin.dxf round 3`, baseline
`binary-base`, 7 runs -- 5.27 s / 39.66 G instructions before, 5.34 s / 40.13 G after (+1.2 %), the same
bytes.  (The binary path is slow in itself -- 22 GB allocated for three parses, the values turned to text --
which is another matter than this move.)

The last row is every program again on the code as it stands (load average 15-17 while it ran: CPU is up
12-26 % on every row, the write and print rows included, whose code did not change).  The printer's and the
writer's instructions move by 0.4-0.6 % with no change to their code, which is how much this measure moves on
its own here; f001's parse is +2.6 % (it was +1.8 % right after step 3), the small files +1.8 % and 0.0 %.
Allocation and peak are unchanged on every row, and every output is the SAME bytes as the baseline's.

Tried and NOT moved at first (moved since, below): the number reader (`DXF.Number`, its `at` is
`unsafeIndex`) on `index8`: f001 8.70 G instructions, +2.7 % for this change alone and +4.6 % against the
baseline (CPQ +3.0 %) -- every digit of every real is read through it, and each read is checked again
after the loop's own `j < n`.  Reverted then.

### After linbuf 208aafd: `sliceAt`, `index8` without `runRW#`

linbuf answered 06d57ff7 with `LinBuf.Index.sliceAt b i n` (two unsigned compares, the failure out of
line, always a `BS` over the same `ForeignPtr`), and `index8` no longer runs `runRW#` per read.  Each
retry is held against HEAD built on the SAME new linbuf (`bench` with `against`, 5 runs; load average
20-25, so instructions are the figure), and its output against the baseline's bytes.

`Tag.Text.slice` on `sliceAt`: first as a drop-in, f001 +5 % instructions, +2 % allocation -- the failure
branch makes the slice no longer cheap enough to build speculatively, so the value of every `Tag` became
a thunk.  Forced where the tag is built (`let !v = slice ...`): f001 8.31 G against 8.50 G (0.98x), CPU
equal, PEAK 118 MB against 179 MB (no thunk per tag held); CPQ-0005 1.00x instructions.  SAME bytes,
the whole check passes.  `Tag.Text` has no `unsafe` read or slice left.

`DXF.Number` (the number reader, NOT moved before at +2.7 %): with the new `index8` as a drop-in for `at`,
f001 +2.4 % instructions, CPQ +0.6 %.  Read instead with `peek8` (the byte, or -1 past the end: one
unsigned compare) the loops lose their own `j < n` -- -1 is no digit, sign, point, exponent letter or
blank, so every guard stops on it as it stopped at the end: f001 8.37 G against 8.39 G (1.00x), CPQ-0005
8.16 / 8.14, condo A-301 7.88 / 7.85, CPU equal; SAME bytes on all three, the whole check passes (the
300,000 correctly-rounded reads among it).  `at` (index8) stays only in the slow exact path.

### Step 4: the printer's stores on `LinBuf.Fixed` (after linbuf c766ab2)

At first not done: the printer's primitives (`DXF.Tag.Prim`, used by `DXF.Entity.FastOut` and
`DXF.Entity.Common.headWriter`) are `BoundedPrim`s run inside a `Data.ByteString.Builder` (every codec
writes `Out = Fmt -> Builder`), and a linbuf `Bounded` ran only on a linbuf `Buf`.  Reported (b0b82522);
linbuf c766ab2 answered with `LinBuf.Builder.asFixedPrim`/`asBoundedPrim`: a `Fixed`/`Bounded` as a
builder's primitive, storing at the builder's address.

Every store `DXF.Tag.Prim` made by hand is now linbuf's: the integers (`F.intDec`, two digits a
division, replacing dxf's digit-at-a-time `putNat`), the line ends (`u8 10`, `u16le 0x0A0D`), the code
padding (`u8 32`, `u16le 0x2020`) and a real zero's `0.0` (`u16le <+> u8`); each made a bytestring
primitive by `asFixedPrim`/`asBoundedPrim` and run at the tag's address by bytestring's `runF`/`runB`, inside
the one `boundedPrim` that writes the whole tag (or the whole entity head), so a tag is still one call.
No `pokeByteOff` is left.  A tag is not ONE linbuf `Bounded`: its shape is chosen at run time (padding
by the code, CR LF or LF) and a `Bounded`'s size is in its type, with no choice or widening.

Against HEAD on the same linbuf (5-7 runs, load 11-16): f001 printloop 28.16 G instructions against
29.46 (0.96x), allocation 4165 -> 3553 MB; f001 round 3 8.17 / 8.32 (0.98x), 810 MB allocated against
872; write 100k R2010 5.36 / 5.38, R12 4.90 / 4.89; CPQ-0005 8.09 / 8.16.  CPU swung -16 % to +30 % between
reruns of the same pair.  SAME bytes on every output (f001, CPQ, both writes); the whole check passes.

Not moved at this step: a short string's copy (linbuf then had no `Bounded` copying up to n bytes of a
`ByteString`) and the reals (below).

The short string's copy, after linbuf 25c7637 (`bytesUpTo`, answering 29e3f67f): `putBytesTag` runs
`asBoundedPrim (bytesUpTo @64)`, so the last `unsafeUseAsCString` + `copyBytes` in `Tag.Prim` is gone.
A/B against HEAD (d378070), the least CPU of 5-7 runs, load about 10:

| program | G instr ours / HEAD | CPU | bytes |
|---|---|---|---|
| f001 printloop | 24.78 / 25.45 (0.97x) | 0.96x | -- |
| f001 round 3 | 7.81 / 7.89 (0.99x) | 0.81x | SAME |
| write 100k R2010 | 5.27 / 5.30 (0.99x) | 1.12x (noise: instructions 0.99x) | SAME |
| CPQ-0005 round 60 | 8.05 / 8.04 (1.00x) | 0.99x | SAME |

The whole check passes (43 of 43).

### The reals on `LinBuf.Real.shortestDigits` (the user's decision, 2026-10-14)

**From this commit the output bytes change by design: shorter digits, the same values.**  The user allowed
linbuf's `shortestDigits` (Schubfach: the fewest digits that read back, the closest of those, ties to even)
to replace dxf's Grisu2 if every real reads back to the same double.  `DXF.Tag.Double` is now dxf's layout
(plain decimal, `I.F`, at least one fraction digit) over `shortestDigits`: the integer part `wordDec`, the
point, the fraction `wordDecPadded` -- one linbuf `Bounded` a layout, run at the builder's address by
`asBoundedPrim`.  The choice between layouts (a sign or none; below 1e-4, where the fraction can be 21
digits and `wordDecPadded` stops at 20, `0.0000` then the digits) is bytestring's `condB`: linbuf has no
choice between two `Bounded`s of different sizes (requested in 29e3f67f).  Grisu2, its cached powers and
its `Ptr` digit loop are gone.

Held against the previous printer on all 93 samples and the 20 fixtures (`Check.Reals.realsReport`: each
file printed by the HEAD binary against the new print, tag by tag):

* 258 of 946,955 reals changed text, all in one file (f001 / `3CG`); every other file the same bytes.
  240 shorter, 18 the same length (the closer of two shortest), none longer.  E.g.
  `23262.283155407298 -> 23262.2831554073`, `6620.1543850411535 -> 6620.154385041154`,
  `174956.28344109101 -> 174956.283441091`, `79231.96733296097 -> 79231.96733296096`.
* every changed real reads back (dxf's `readDouble`) to the bit-identical double of its old text; no tag but
  a real differs; the tag codes are the same.
* every file's new print parses back to the original's model (`parse (print d) == d`), and the whole
  check passes (43); the doubles check now requires the shortest digits for every sample real (it allowed
  one in 200 longer under Grisu2).

Against HEAD (1e055be) on the same linbuf (5 runs, least CPU, load 7-10):

| program | CPU s | G instr | ratio (instr) | bytes |
|---|--:|--:|--:|---|
| f001 printloop | 2.73 / 3.59 | 25.44 / 28.11 | 0.91 | 258 reals shorter or closer |
| f001 round 3 | 0.86 / 0.97 | 7.89 / 8.15 | 0.97 | (the same 258) |
| CPQ-0005 round 60 | 0.89 / 0.90 | 8.07 / 8.07 | 1.00 | SAME |
| write 100k R2010 | 0.69 / 0.69 | 5.30 / 5.33 | 0.99 | SAME |
| write 100k R12 | 0.61 / 0.62 | 4.84 / 4.86 | 1.00 | SAME |

Allocation and peak unchanged (f001 round 3: 807 MB, 118 MB).

The choice of layout, after linbuf 25c7637 (`choose`, `widen`, answering 29e3f67f): `condB` is gone; a
real is one linbuf `Bounded 42` -- `choose (x < 0)` between the sign and the magnitude, and the plain
layout, the magnitude `choose (a < 1e-4)` between `below` widened from 26 to 41 and `plain`.  Against
HEAD (4ebc02f), least CPU of 5-7 runs, load 8-12: f001 printloop 24.84 / 24.77 G instr (1.00x, CPU 1.00x),
f001 round 3 7.86 / 7.88 (1.00x), write 100k R2010 5.30 / 5.29 (1.00x), CPQ-0005 round 60 8.04 / 8.03
(1.00x); the same bytes; the whole check passes.

Tried and not moved: a whole tag as one linbuf `Bounded` (`padB <+> intDec code <+> eolB <+> value <+> eolB`,
padding and line end each a `choose` between widened stores, as linbuf's reply to 29e3f67f suggested), run
by `asBoundedPrim` in place of `Tag.Prim`'s `Ptr` steps.  The same bytes and the whole check, but against
HEAD (b2d578d): f001 printloop 37.51 / 24.75 G instr (1.52x), CPU 1.83x, allocation 17.1 GB against 3.0;
f001 round 3 1.16x instr (2.2 GB allocated against 0.76); writes 1.02x; CPQ-0005 1.01x.  The tag of
`choose`s and `<+>`s is built per tag (a closure for each store, the choice not made at compile time),
where the `Ptr` steps cost nothing.  Reverted; reported to linbuf (66c0eb26).

#### 4d. A whole tag as one `Bounded`, again (linbuf 2f6e1b8)

linbuf found one cause (66c0eb26, fixed in 2f6e1b8): `intDec`'s sign was a branch around its `Bounded`,
so a composed tag passed each side's store to a join point as a closure.  At 2f6e1b8 the same tag still
cost 1.50x instructions and 17.1 GB on f001 printloop: the rest was dxf's.  A profile put 87% of the
allocation in `$wfastOut`, and its Core showed why: `Tag.Double`'s `real` bound `shortestDigits`'s triple,
the `(i, f)` split and the fraction's length in a lazy `where`, and a `Bounded` is a value with TWO stores
(to an array, to an address) that share what it is built from -- so each became a thunk per real, and the
digits were taken twice (for `x` and `negate x`).  Two changes in dxf:

* `real` takes the digits of `abs x` once, strictly (a `case` with bangs), before any `choose`; `plain`
  and `below` take the digits, not the double, and split them strictly.  (The rule: what a `Bounded` is
  built from is evaluated before it, never bound lazily around it -- the same rule as linbuf's header,
  one level up.)  f001 printloop: 17.1 -> 3.5 GB, 1.50x -> 1.03x instructions.
* The three `Ptr` writers `Entity.FastOut` calls (`putDoubleTag`, `putIntTag`, `putBytesTag`) are
  `NOINLINE`: each tag's whole layout (both line ends, three paddings, every real layout) was copied into
  every place FastOut writes a tag -- its Core went from 6.8k lines to 53k.  Compiled once, FastOut's Core
  is 2.2k lines and a tag is one call, as before.

Now `Tag.Prim` has no address arithmetic: a tag is `codeB pad crlf c <+> value <+> eolB crlf` (a real's
`choose (x == 0) zero (real x)`), run by `asBoundedPrim`; `fixedAt`, `plusPtr`, `runF` and bytestring's
`boundedPrim` are gone from it.  A/B against HEAD (ab6f0ec, built on linbuf 2f6e1b8 too), the least CPU of
5-9 runs, load 6-9:

| program | G instr ours / HEAD | ratio | allocated ours / HEAD | CPU ratio |
|---|---|---|---|---|
| f001 printloop | 25.58 / 25.12 | 1.02x | 2.54 / 3.18 GB | 1.09x |
| f001 round 3 | 7.92 / 7.86 | 1.01x | 727 / 792 MB | 1.15x |
| write 100k R2010 | 5.35 / 5.25 | 1.02x | 967 MB | 1.02x |
| write 100k R12 | 4.88 / 4.83 | 1.01x | 925 MB | 0.90x |
| CPQ-0005 round 60 | 8.14 / 8.04 | 1.01x | 931 MB | 0.88x |

Instructions within 3% everywhere, allocation down; CPU swings both ways with the load (HEAD's own
printloop CPU ran 2.22-2.71 s in these runs).  The same bytes on all four outputs; the whole check (43)
passes.  Moved.

Tried and NOT moved: an entity as one `Bounded` of tags (the step after 4d).  `Common.headWriter` became
`headB`, the plain head as `bytesTagB 0 ty <+> optBytesB hc handle <+> ... <+> choose graphic (...)`, each
optional tag a `choose` on whether it is there (bound strictly), run by one `asBoundedPrim`; `FastOut`'s
LINE, VERTEX, POLYLINE and INSERT writers the same, so `Tag.Prim`'s `Ptr` writers and `runB` were gone.
The same bytes on every output and the whole check, but against HEAD (a9738a8, same linbuf), least CPU of
3-5 runs, load 7-10:

| program | G instr ours / HEAD | ratio |
|---|---|---|
| f001 printloop | 25.67 / 26.09 | 0.98x |
| f001 round 3 | 7.90 / 7.94 | 0.99x |
| write 100k R2010 | 5.44 / 5.35 | 1.02x |
| write 100k R12 | 4.96 / 4.93 | 1.01x |
| CPQ-0005 round 60 | 8.69 / 8.20 | **1.06x** |
| condo A-301 round 60 | 8.37 / 7.92 | **1.06x** |

f001 is LINEs (the `FastOut` shapes); the small files are mostly other entities, whose plain head is
now nine `choose`s evaluated at run time where the `Ptr` steps skipped an absent tag with one `maybe`:
+6 % on the whole round, so about +15-20 % on their print.  Over 3 %: reverted, and the `Ptr` positions
stay (the patch kept as `.ghci-session/entity-bounded.patch`, not committed).  Per AGENTS.md this is where
the printer's move stops.

### What is left

| module | what | why it stays |
|---|---|---|
| `Tag.Text` (`codeLine`) | `unsafeShiftR` | a shift by `8 * k`, `k < 8`: never past the word; no memory read. Right as it is |
| `Number` | `MagicHash` (`Double#` in an unboxed sum) | the unboxed sum is what keeps a read real from being allocated; no memory access. Right as it is |
| `Number.Lemire` | `unsafeAt` (the power-of-ten table, its index range-checked just before), `unsafeShift*`, `timesWord2#` | arithmetic and a constant table, not a buffer: nothing of linbuf's. Right as it is |
| `Tag.Prim` | `runB` and `Ptr Word8` in the types of `putDoubleTag`/`putIntTag`/`putBytesTag` | a tag is one linbuf `Bounded` (4d); `runB` hands it to `Entity.FastOut`/`Entity.Common`, which write several tags in one primitive at a `Ptr`.  linbuf has no `Ptr`-taking runner for a `Bounded` (b0b82522 (b), not done) |
| `Entity.FastOut`, `Entity.Common` | `Ptr Word8` positions, `boundedPrim` | an entity (a LINE's head and numbers, a plain head) is one primitive of several tags.  As one `Bounded` of tags (each optional tag a `choose`) it cost +6 % instructions on the small samples (above): not moved.  What linbuf would need: a `Ptr`-taking runner for a `Bounded` (b0b82522 (b)) so these stay as they are without bytestring's `runB` |
| `Entity.Fast` (`afterType`) | `unsafeForeignPtrToPtr`, `minusPtr` | the offset of a tag's value inside the input, from the two addresses; nothing is read through them, and the offset is verified (`slice bs off e == ty`, bounds checked) before use. linbuf has no "offset of a slice in its parent"; right as it is |
| `Entity.FastKit` | `MagicHash` (`D#`) | builds a `Double` from the number reader's unboxed result; no memory access. Right as it is |
| `Intern` | `unsafeDupablePerformIO` | `intern` is pure over a table made for one parse (interning does not change a value). Right as it is |
| `Parse` | `unsafePerformIO` | `try`/`evaluate` turning the parser's `DxfError` into `Either`, and the per-parse interning table. Right as it is |

Moved: `Tag.Text` (the tag reader: `index64le`, `index8`, `sliceAt`), `Tag.Binary` (`index8`), `Number` (`peek8`, `index8`), `Tag.Prim` (`LinBuf.Fixed` stores through `LinBuf.Builder`), `Tag.Double` (`LinBuf.Real.shortestDigits`, `wordDec`, `wordDecPadded`), `Intern` (`LinBuf.Ints`,
`readMasked`/`writeMasked`).
