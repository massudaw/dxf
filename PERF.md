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

## What is left

* Fast decoders and writers for the other usual entities (VERTEX, LWPOLYLINE, TEXT, INSERT, CIRCLE, ARC):
  the small samples parse at about 34 MB/s, below the 100 MB/s aim; only LINE-heavy files reach it.
* Doubles are formatted again (shortest digits, which reproduces CAD output byte for byte) rather than
  copied from the source bytes: a copy would cost 20+ bytes per number held in memory.
* The printed f001 parses in 172 ms against 140 ms for the original (long digit runs fall to the slower
  path); not investigated.
