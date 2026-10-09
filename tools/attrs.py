"""List the subclasses and attributes (name:code) an ezdxf entity class has.
usage: attrs.py TYPE [TYPE...]   e.g. attrs.py TEXT LINE
"""
import sys
from ezdxf.entities import factory
for t in sys.argv[1:]:
    cls = factory.ENTITY_CLASSES.get(t)
    if cls is None:
        print(t, "-- no ezdxf class"); continue
    print("==", t, cls.__name__, "graphic" if hasattr(cls, "DXFTYPE") and "DXFGraphic" in [c.__name__ for c in cls.__mro__] else "object")
    mod = sys.modules[cls.__module__]
    for k, v in vars(mod).items():
        if k.startswith("acdb_") or k in ("base_class",):
            if hasattr(v, "attribs") and isinstance(v.attribs, dict) and k not in ("base_class", "acdb_entity"):
                items = []
                for n, a in v.attribs.items():
                    x = getattr(a, "xtype", None)
                    items.append(f"{n}:{a.code}{'' if x is None else '/' + str(x).split('.')[-1]}{'' if a.default is None else '=' + repr(a.default)[:20]}")
                print(f"  {k} [{v.name}] ", " ".join(items))
