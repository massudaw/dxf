"""Oracle for the builder (DXF.Build): what ezdxf finds in a file the builder wrote.

  build_check.py describe FILE   audit result, layers, apps, entities of
                                 model space and blocks with their extended
                                 data, one canonical line each
  build_check.py rewrite IN OUT  ezdxf reads IN and writes OUT

Run with the reference's Python:  .ref/venv/bin/python -I tools/build_check.py ...
"""
import sys
import ezdxf


def fmt(code, v):
    if code == 1040 or code == 1041 or code == 1042:
        return "%d:%r" % (code, float(v))
    if isinstance(v, (tuple, list)) or hasattr(v, "x"):
        return "%d:%s" % (code, ",".join(repr(float(c)) for c in v))
    return "%d:%s" % (code, v)


def xdata(e):
    out = []
    xd = e.xdata
    if xd is None:
        return ""
    for app, tags in xd.data.items():
        out.append(app + "=" + ",".join(fmt(t.code, t.value) for t in tags[1:] if t.code != 1001))
    return ";".join(out)


def ent(e):
    s = "ENT %s %s %s" % (e.dxftype(), e.dxf.layer, xdata(e))
    if e.dxftype() == "TEXT":
        s += " " + e.dxf.text
    if e.dxftype() == "INSERT":
        s += " " + e.dxf.name
    if e.dxftype() in ("POLYLINE",):
        s += " v=%d" % len(list(e.vertices))
    if e.dxftype() == "LWPOLYLINE":
        s += " v=%d" % len(e)
    return s


def describe(path):
    doc = ezdxf.readfile(path)
    aud = doc.audit()
    print("AUDIT errors %d fixes %d" % (len(aud.errors), len(aud.fixes)))
    for m in aud.errors + aud.fixes:
        print("AUDIT-MSG %s" % m.message)
    print("VERSION", doc.dxfversion)
    for l in sorted(doc.layers, key=lambda l: l.dxf.name):
        print("LAYER %s %d %s" % (l.dxf.name, abs(l.dxf.color), l.dxf.linetype))
    for a in sorted(a.dxf.name for a in doc.appids):
        print("APPID", a)
    for e in doc.modelspace():
        print(ent(e))
    for b in doc.blocks:
        if b.name[0] in "*$":
            continue
        print("BLOCK", b.name)
        for e in b:
            print(ent(e))


if __name__ == "__main__":
    if sys.argv[1] == "describe":
        describe(sys.argv[2])
    elif sys.argv[1] == "rewrite":
        ezdxf.readfile(sys.argv[2]).saveas(sys.argv[3])
