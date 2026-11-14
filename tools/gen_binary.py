"""Fixtures for the tag layer: the same document written as text and as binary DXF by ezdxf,
in R12, R2007 and R2018: test/data/{r12,r2007,r2018}.{txt,bin}.dxf.
The test compares the tags of the binary file with those of the text file.
Run:  .ref/venv/bin/python -I tools/gen_binary.py   (from the project root)
"""
import ezdxf

for version, name in (("R12", "r12"), ("R2007", "r2007"), ("R2018", "r2018")):
    doc = ezdxf.new(version, setup=True)
    doc.layers.add("Walls", color=3)
    doc.layers.add("Dimensions", color=5, linetype="DASHED")
    doc.styles.add("Plan", font="arial.ttf")
    blk = doc.blocks.new("Marker")
    blk.add_circle((0, 0), 0.5)
    blk.add_line((-1, 0), (1, 0))
    blk.add_line((0, -1), (0, 1))
    msp = doc.modelspace()
    for i in range(40):
        msp.add_line((i * 1.5, 0.1 * i), (i * 1.5 + 1, 2.5 - 0.0625 * i), dxfattribs={"layer": "Walls"})
    msp.add_circle((10, 10), 2.5, dxfattribs={"color": 1})
    msp.add_arc((0, 0), 4, 30, 120)
    msp.add_point((1e-7, -2.5e9, 3.141592653589793))
    msp.add_text("Plan éè ü ° café" if version != "R12" else "Plan cafe",
                 dxfattribs={"height": 0.25, "style": "Plan", "layer": "Dimensions"})
    msp.add_blockref("Marker", (5, 5), dxfattribs={"xscale": 2, "rotation": 45})
    msp.add_lwpolyline([(0, 0, 0, 0, 0.5), (4, 0), (4, 3), (0, 3)], format="xyseb", close=True) \
        if version != "R12" else msp.add_polyline2d([(0, 0), (4, 0), (4, 3), (0, 3)], close=True)
    if version != "R12":
        msp.add_mtext("First line\\PSecond line", dxfattribs={"char_height": 0.2})
        msp.add_spline([(0, 0), (1, 2), (3, 3), (4, 0)])
        h = msp.add_hatch(color=2)
        h.paths.add_polyline_path([(0, 0), (2, 0), (2, 2), (0, 2)], is_closed=True)
    doc.saveas(f"test/data/{name}.txt.dxf")
    doc.saveas(f"test/data/{name}.bin.dxf", fmt="bin")
