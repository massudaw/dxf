"""Fixtures for the table entries: test/data/tables.dxf (R2018) and tables_r12.dxf (R12).
Run from the project root:  .ref/venv/bin/python -I tools/gen_tables.py
"""
import ezdxf

for version, name in (("R2018", "tables"), ("R12", "tables_r12")):
    doc = ezdxf.new(version, setup=True)
    r12 = version == "R12"
    doc.layers.add("Walls", color=3, linetype="DASHED", dxfattribs={"flags": 4})
    doc.layers.add("Off", color=5).off()
    if not r12:
        l = doc.layers.add("Rich", color=7, dxfattribs={"true_color": 0x123456, "lineweight": 35, "plot": 0})
    doc.linetypes.add("MYDASH", pattern=[1.5, 1.0, -0.5], description="my dashes")
    if not r12:
        doc.linetypes.add("WITHSHAPE", pattern=[1.0, 0.25, -0.25, -0.5], description="text") 
    doc.styles.new("MYFONT", dxfattribs={"font": "arial.ttf", "height": 0.5, "width": 0.9, "oblique": 10, "last_height": 3})
    doc.dimstyles.new("MYDIM", dxfattribs={"dimtxt": 3.5, "dimasz": 1.8, "dimtad": 1, "dimdec": 3, "dimscale": 2})
    doc.appids.new("MYAPP")
    doc.ucs.new("MYUCS", dxfattribs={"origin": (1, 2, 3), "xaxis": (0, 1, 0), "yaxis": (-1, 0, 0)})
    doc.views.new("MYVIEW", dxfattribs={"height": 100, "width": 200, "center": (5, 5), "target": (0, 0, 1), "direction": (0, 0, 1)})
    doc.viewports.new("MYPORT", dxfattribs={"height": 400, "aspect_ratio": 1.5, "center": (2, 3), "snap_on": 1, "grid_on": 1})
    doc.saveas(f"test/data/{name}.dxf")
