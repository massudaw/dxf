# Performance

Apple M-series laptop, GHC 9.14.1 `-O2`, single thread.  Library side: `dxf-bench FILE [doc] +RTS -s`
(wall time of each stage; "doc" skips the tag-layer stages; max residency is GHC's live data, peak RSS
from `/usr/bin/time -l`).  ezdxf 1.4.4 on Python 3.14: `tools/perf_ezdxf.py` (peak RSS includes the
~55 MB of the interpreter and its imports).

## Whole document: parse to typed entities, print back

"parse" is bytes to a fully evaluated `Document` (scan, tables, blocks, typed entities).  "print" is
`Document` to bytes.  Both outputs are checked: the printed file parses to the same `Document`.

| file | size | parse | MB/s | print | max residency | peak RSS | ezdxf read | ezdxf write | ezdxf RSS |
|------|-----:|------:|-----:|------:|--------------:|---------:|-----------:|------------:|----------:|
| f001.dxf (112,322 LINE) | 19.3 MB | 410 ms | 47 | 720 ms | 100 MB | 227 MB | 3.35 s | 3.65 s | 263 MB |
| CPQ-SPS-ZZ-ZZ-DR-A-0005 | 509 KB | 18 ms | 28 | 32 ms | 8.6 MB | 31 MB | 0.251 s | 0.300 s | 62 MB |
| condo_A-301_arch-elevation | 470 KB | 17 ms | 28 | 31 ms | 8.0 MB | 30 MB | 0.228 s | 0.282 s | 62 MB |

The library parses f001 about 8 times faster than ezdxf and prints it about 5 times faster, holding
the parsed file in 5 times its size (ezdxf: 263 MB peak for the whole process).

## The tag layer alone (scan to `[Tag]`, print back, byte for byte)

| file | size | tags | scan | MB/s | print | max residency (list kept) |
|------|-----:|-----:|-----:|-----:|------:|------:|
| f001.dxf | 19.3 MB | 1,349,950 | 191 ms | 100 | 232 ms | 158 MB |

## Where the time goes (f001, cost-centre profile of parse + print)

Nothing dominates: scanning text 8 %, writing tags 7 %, `commonHead` 7 %, formatting doubles
(`Builder.doubleDec`) about 20 % of the print, reading doubles 3 %, the rest spread over the codec
folds.  Parsing is half scanning (190 ms) and half building the typed entities (200 ms).  The
target of 100 MB/s holds for the tag layer; for the typed document it is at 47 MB/s.  What would
help next: a decoder that reads a LINE's fixed tag order without the generic field table, and
writing doubles from the source bytes when the value has not changed.
