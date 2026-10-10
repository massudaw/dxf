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

## What is left

* The small files parse at 46-74 MB/s (cold single run / steady state), below the 100 MB/s aim; f001
  reaches it. What remains is allocation (40 bytes per input byte) in the FastKit reader monad and the
  per-entity records; unboxed-sum return types or a fixed-shape VERTEX decoder are the next step.
* Doubles are formatted again (shortest digits, which reproduces CAD output byte for byte) rather than
  copied from the source bytes: a copy would cost 20+ bytes per number held in memory.

