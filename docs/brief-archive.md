# Brief archive

History and reference moved out of AGENTS.md (every call carries the brief); text verbatim from the old AGENTS.md. The standing rules, commands and open work stay in AGENTS.md.

## What to build, in this order (the steps)

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

## Part four: the tables on membudget

The user's word (2026-10-14): dxf moves its memo / intern / cache tables onto **membudget**
(`~/code/membudget`: the user's own package -- interners, memos, bounded caches and frozen tables, each with a
memory budget, no `unsafe*` in the caller; it depends on base, bytestring, containers and linbuf, all allowed).
Its agent ported this project's tables as COPIES and measured them: `~/code/membudget/docs/adopting.md`,
"## dxf" -- what to change, what it gives, how to check -- and `bench/port/` there. membudget is READ ONLY for you:
what it lacks or does wrong is a `report` with `about: membudget`.

`cabal.project` now has `../membudget` (linbuf is there already); add `membudget` to the library's
`build-depends`.

1. **The baseline**: your own (the last part's) programs and samples, outputs kept; plus the PEAK memory of each.
2. **The move**, as adopting.md says, one table a commit: the SAME OUTPUT on every sample, time/instructions within
   3% or better, peak equal or lower -- and BOUNDED: one run with a budget smaller than the table's natural size,
   output still the same, memory held at the budget. Give each table a name in membudget's registry and a budget
   that adopting.md proposes (say the figure in PERF.md).
3. **What is left**: every `unsafePerformIO`/global table still here, each with why it stays.
A table that comes out slower is NOT moved: PERF.md says why, and a report to membudget says what it would need.
Commit and push each; say WAITING when done.
