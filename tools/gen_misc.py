"""Fixtures for the misc group: test/data/misc.dxf (R2018) and misc_r12.dxf (R12, nothing of this exists there).
Run from the project root:  .ref/venv/bin/python -I tools/gen_misc.py
"""
import ezdxf

doc = ezdxf.new("R2018", setup=False)
msp = doc.modelspace()
idef = doc.add_image_def("test.png", size_in_pixel=(640, 480))
msp.add_image(idef, insert=(1, 2), size_in_units=(6.4, 4.8), rotation=15, dxfattribs={"layer": "IMG"})
img = msp.add_image(idef, insert=(0, 0), size_in_units=(3, 3))
img.set_boundary_path([(0, 0), (100, 0), (100, 100), (0, 100)])
msp.add_wipeout([(0, 0), (4, 0), (4, 4), (0, 4)])
layout = doc.layouts.new("Plot")
layout.add_viewport(center=(5, 5), size=(10, 8), view_center_point=(3, 3), view_height=20, status=1)
ud = doc.add_underlay_def("sheet.pdf", name="1")
msp.add_underlay(ud, insert=(1, 1, 0), scale=(2, 2, 2), rotation=10)
doc.saveas("test/data/misc.dxf")

# ACIS entities keep SAT text up to R2010; R2013 and later hold binary SAB data
doc = ezdxf.new("R2010", setup=False)
msp = doc.modelspace()
sat = ["400 1 0 0", "16 Autodesk AutoCAD 24.0 ASM 221.0.0.1832 NT 0 Thu Jan 01 00:00:00 2015", "1 9.9999999999999995e-007 1e-010", "end-of-ACIS-data"]
for t in ("3DSOLID", "BODY", "REGION"):
    e = msp.new_entity(t, {"layer": "ACIS"})
    e.sat = sat
doc.saveas("test/data/misc_r2010.dxf")
