# Performance

Measured on an Apple M-series laptop, GHC 9.14.1 `-O2`, single thread.  `dxf-bench FILE +RTS -s`
for this library (wall time of the stage, "max residency" for memory), `tools/perf_ezdxf.py` under
`/usr/bin/time -l` for ezdxf 1.4.4 on Python 3.14 (peak RSS includes the ~55 MB the interpreter and
the import cost).

## Stage 1: the tag layer (scan to `[Tag]`, print back)

| file | size | tags | scan | MB/s | print | max residency (list kept) |
|------|-----:|-----:|-----:|-----:|------:|------:|
| samples/f001.dxf | 19.3 MB | 1,349,950 | 201 ms | 96 | 240 ms | 158 MB |

ezdxf, same file: read 3.35 s, write 3.65 s, peak RSS 263 MB.

(Work in progress: this table is updated as each stage lands.)
