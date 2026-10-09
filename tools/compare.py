"""Compare what this library found in a DXF file with what ezdxf finds.

usage: compare.py FILE.dxf SUMMARY.json [--verbose]

SUMMARY.json is written by DXF.Summary.  Exit status 0 when everything the
library found agrees with ezdxf and the library has every attribute ezdxf
shows for the types it has a codec for; otherwise the differences are
printed and the status is 1.  A line "UNTYPED type=count ..." tells which
entity types the library keeps as tags only.

Attributes are named as ezdxf names them (dxf.start, dxf.center ...).
Types with parts ezdxf does not show as attributes (the vertices of an
LWPOLYLINE) add them in tools/oracle_ext/*.py: a dict EXT mapping a DXF
type name to a function entity -> {name: value}.
"""
import sys, json, math, glob, os, importlib.util, collections
import ezdxf
from ezdxf import recover
from ezdxf.lldxf.encoding import decode_dxf_unicode
from ezdxf.math import Vec2, Vec3

IGNORE = {"handle", "owner"}  # ezdxf invents these for files that have none
# header variables ezdxf sets itself when it loads a file
HEADER_IGNORE = {"$TDCREATE", "$TDUCREATE", "$TDUPDATE", "$TDUUPDATE", "$FINGERPRINTGUID", "$VERSIONGUID", "$ACADMAINTVER"}
TOL = 1e-9


def load_ext():
    ext = {}
    here = os.path.dirname(os.path.abspath(__file__))
    for p in sorted(glob.glob(os.path.join(here, "oracle_ext", "*.py"))):
        spec = importlib.util.spec_from_file_location(os.path.basename(p)[:-3], p)
        m = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(m)
        ext.update(getattr(m, "EXT", {}))
    return ext


EXT = load_ext()


def norm(v):
    """An ezdxf value as plain data."""
    if isinstance(v, (Vec3, Vec2)):
        return [float(x) for x in v]
    if isinstance(v, bool):
        return int(v)
    if isinstance(v, (list, tuple)):
        return [norm(x) for x in v]
    if isinstance(v, dict):
        return {k: norm(x) for k, x in v.items()}
    if isinstance(v, bytes):
        return v.decode("latin-1")
    return v


def same(ours, theirs, enc):
    """Does our JSON value agree with ezdxf's?"""
    if isinstance(theirs, str):
        if not isinstance(ours, str):
            return False
        try:
            a = decode_dxf_unicode(ours.encode("latin-1").decode(enc, "replace"))
        except Exception:
            a = ours
        return a == decode_dxf_unicode(theirs) or ours == theirs
    if isinstance(theirs, (int, float)) and not isinstance(theirs, bool):
        if not isinstance(ours, (int, float)):
            return False
        return math.isclose(ours, theirs, rel_tol=TOL, abs_tol=TOL)
    if isinstance(theirs, list):
        if not isinstance(ours, list):
            return False
        n = min(len(ours), len(theirs))
        if abs(len(ours) - len(theirs)) > 0 and not (n in (2, 3) and len(ours) in (2, 3) and len(theirs) in (2, 3)):
            return False
        return all(same(a, b, enc) for a, b in zip(ours, theirs))
    if isinstance(theirs, dict):
        return isinstance(ours, dict) and all(k in ours and same(ours[k], v, enc) for k, v in theirs.items())
    return ours == theirs


class Report:
    def __init__(self):
        self.problems = []
        self.missing = collections.Counter()
        self.untyped = collections.Counter()
        self.typed = collections.Counter()

    def bad(self, msg):
        self.problems.append(msg)


def attribs_of(e):
    d = {k: norm(v) for k, v in e.dxf.all_existing_dxf_attribs().items()}
    f = EXT.get(e.dxftype())
    if f:
        d.update({k: norm(v) for k, v in f(e).items()})
    return d


def kids_of(e):
    t = e.dxftype()
    ks = []
    if t == "POLYLINE":
        ks = list(e.vertices)
    elif t == "INSERT":
        ks = list(e.attribs)
    sq = getattr(e, "seqend", None)
    if sq is not None and (ks or t == "POLYLINE"):
        ks.append(sq)
    return ks


def compare_entity(rep, where, ours, theirs, enc):
    t = theirs.dxftype()
    if ours["type"] != t:
        rep.bad(f"{where}: type {ours['type']} but ezdxf has {t}")
        return
    oa = ours["attribs"]
    if "raw" in oa:
        rep.untyped[t] += 1
        return
    rep.typed[t] += 1
    ta = attribs_of(theirs)
    for k, v in ta.items():
        if k in oa:
            if k != "owner" and not same(oa[k], v, enc):
                rep.bad(f"{where} {t}.{k}: ours {oa[k]!r} ezdxf {v!r}")
        elif k not in IGNORE:
            rep.missing[(t, k)] += 1
    ok = ours.get("kids", [])
    tk = kids_of(theirs)
    if len(ok) != len(tk):
        rep.bad(f"{where} {t}: {len(ok)} children but ezdxf has {len(tk)}")
    else:
        for i, (a, b) in enumerate(zip(ok, tk)):
            compare_entity(rep, f"{where}/{i}", a, b, enc)


def compare_list(rep, where, ours, theirs, enc, allow_extra=False):
    """Entities by handle when both sides have them, else by position."""
    oh = [o["attribs"].get("handle") for o in ours]
    th = [t.dxf.handle if t.dxf.hasattr("handle") else None for t in theirs]
    if ours and all(oh) and len(set(oh)) == len(oh) and all(th):
        tmap = {h: t for h, t in zip(th, theirs)}
        for o in ours:
            t = tmap.get(o["attribs"]["handle"])
            if t is None:
                rep.bad(f"{where}: handle {o['attribs']['handle']} ({o['type']}) not in ezdxf")
            else:
                compare_entity(rep, f"{where}[{o['attribs']['handle']}]", o, t, enc)
        if len(ours) != len(theirs) and not allow_extra:
            rep.bad(f"{where}: {len(ours)} entries but ezdxf has {len(theirs)}")
        return
    if len(ours) != len(theirs):
        rep.bad(f"{where}: {len(ours)} entries but ezdxf has {len(theirs)}")
        return
    for i, (o, t) in enumerate(zip(ours, theirs)):
        compare_entity(rep, f"{where}[{i}]", o, t, enc)


TABLES = {
    "LAYER": "layers", "LTYPE": "linetypes", "STYLE": "styles", "DIMSTYLE": "dimstyles",
    "VPORT": "viewports", "VIEW": "views", "UCS": "ucs", "APPID": "appids",
    "BLOCK_RECORD": "block_records",
}


def main():
    path, jpath = sys.argv[1], sys.argv[2]
    verbose = "--verbose" in sys.argv
    ours = json.load(open(jpath))
    try:
        doc = ezdxf.readfile(path)
    except Exception:
        doc, _ = recover.readfile(path)
    enc = "utf-8" if doc.dxfversion >= "AC1021" else doc.encoding
    rep = Report()
    if ours["version"] != doc.dxfversion:
        rep.bad(f"version {ours['version']} vs {doc.dxfversion}")
    # header: the same variables, the same values where ezdxf can say
    names = set(doc.header.varnames())
    mine = set(ours["header"])
    data = open(path, "rb").read()
    for n in sorted(names - mine):
        if n.encode() in data:  # ezdxf adds variables a file does not have
            rep.bad(f"header: ezdxf has {n}, the library does not")
    for n in sorted(mine - names):
        if n not in ("$ACADVER",):
            rep.bad(f"header: the library has {n}, ezdxf does not")
    for n in sorted((mine & names) - HEADER_IGNORE):
        v = norm(doc.header[n])
        vals = ours["header"][n]
        try:
            if isinstance(v, list):
                o = [float(x) for x in vals]
            elif isinstance(v, (int, float)):
                o = float(vals[0])
            else:
                o = vals[0]
        except Exception:
            rep.bad(f"header {n}: cannot read {vals!r} against {v!r}")
            continue
        if not same(o, v, enc):
            rep.bad(f"header {n}: ours {o!r} ezdxf {v!r}")
    # tables
    for tname, entries in ours["tables"].items():
        attr = TABLES.get(tname)
        if attr is None:
            continue
        table = getattr(doc, attr)
        if entries and all("raw" in o["attribs"] for o in entries):
            rep.untyped["table " + tname] += len(entries)
            continue
        theirs = collections.defaultdict(list)
        for e in table:
            theirs[e.dxf.get("name", "")].append(e)
        mine_by = collections.defaultdict(list)
        for o in entries:
            mine_by[o["attribs"].get("name", "")].append(o)
        for n in set(theirs) | set(mine_by):
            a, b = mine_by.get(n, []), theirs.get(n, [])
            if len(a) != len(b):
                rep.bad(f"table {tname}: {len(a)} entries named {n!r} but ezdxf has {len(b)}")
            else:
                for i, (x, y) in enumerate(zip(a, b)):
                    compare_entity(rep, f"table {tname}[{n}]", x, y, enc)
    # blocks
    theirs_blocks = {b.name.lower(): b for b in doc.blocks}
    for b in ours["blocks"]:
        bn = b["name"]
        if bn.lower().startswith(("$model_space", "$paper_space")):  # R12 spelling of *Model_Space
            bn = "*" + bn[1:]
        tb = theirs_blocks.get(bn.lower())
        if tb is None:
            rep.bad(f"block {b['name']!r} not in ezdxf")
            continue
        if bn.lower().startswith(("*model_space", "*paper_space")):
            continue  # ezdxf moves ENTITIES into these; its names differ in R12
        compare_entity(rep, f"block {b['name']!r} head", b["head"], tb.block, enc)
        compare_list(rep, f"block {b['name']!r}", b["entities"], list(tb), enc)
    compare_list(rep, "entities", ours["entities"], list(doc.entities), enc)
    if ours["objects"]:
        compare_list(rep, "objects", ours["objects"], list(doc.objects), enc, True)
    # report
    for p in rep.problems[:40]:
        print("DIFF", p)
    if len(rep.problems) > 40:
        print("DIFF ...", len(rep.problems) - 40, "more")
    for (t, k), n in sorted(rep.missing.items()):
        print(f"MISSING {t}.{k} x{n}")
    print("TYPED", " ".join(f"{t}={n}" for t, n in sorted(rep.typed.items())))
    print("UNTYPED", " ".join(f"{t}={n}" for t, n in sorted(rep.untyped.items())))
    sys.exit(1 if rep.problems or rep.missing else 0)


main()
