# Performance

Apple M-series laptop, GHC 9.14.1 `-O2`, single thread.  Library side: `dxf-bench FILE [doc] +RTS -s`
(wall time of each stage; "doc" skips the tag-layer stages; max residency is GHC's live data, peak RSS
from `/usr/bin/time -l`).  ezdxf 1.4.4 on Python 3.14: `tools/perf_ezdxf.py` (peak RSS includes the
~55 MB of the interpreter and its imports).

## Whole document: parse to typed entities, print back

"parse" is bytes to a fully evaluated `Document` (scan, tables, blocks, typed entities).  "print" is
`Document` to bytes.  Both outputs are checked: the printed file parses to the same `Document`, and
every sample and fixture is compared with ezdxf (`Check.run`).  Best of three runs.

| file | size | parse | MB/s | print | MB/s | max residency | peak RSS | ezdxf read | ezdxf write | ezdxf RSS |
|------|-----:|------:|-----:|------:|-----:|--------------:|---------:|-----------:|------------:|----------:|
| f001.dxf (112,322 LINE) | 19.3 MB | 138 ms | 140 | 85 ms | 228 | 43 MB | 83 MB | 3.35 s | 3.65 s | 263 MB |
| CPQ-SPS-ZZ-ZZ-DR-A-0005 | 509 KB | 15 ms | 34 | 19 ms | 27 | 2.8 MB | 20 MB | 0.251 s | 0.300 s | 62 MB |
| condo_A-301_arch-elevation | 470 KB | 14 ms | 34 | 18 ms | 26 | 2.7 MB | 19 MB | 0.228 s | 0.282 s | 62 MB |

Before this round (commit 54a1d27): f001 parse 410 ms (47 MB/s), print 720 ms, residency 100 MB,
RSS 227 MB; CPQ 18/32 ms, condo 17/31 ms.  f001 is now parsed 24 times and printed 43 times faster than
ezdxf, and a parsed file is held in 2.2 times its size (RSS 4.3 times).  The two small files are
POLYLINE/VERTEX/TEXT drawings that go through the general field-table codecs: they are the next target.

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

## What is left

* Fast decoders and writers for the other usual entities (VERTEX, LWPOLYLINE, TEXT, INSERT, CIRCLE, ARC):
  the small samples parse at about 34 MB/s, below the 100 MB/s aim; only LINE-heavy files reach it.
* Doubles are formatted again (shortest digits, which reproduces CAD output byte for byte) rather than
  copied from the source bytes: a copy would cost 20+ bytes per number held in memory.
* The printed f001 parses in 172 ms against 140 ms for the original (long digit runs fall to the slower
  path); not investigated.
