"""Fixtures for the geometry group: test/data/geometry.dxf (R2018) and geometry_r12.dxf (R12).
Run:  .ref/venv/bin/python -I tools/gen_geometry.py   (from the project root)
"""
import ezdxf

for version, name in (("R2018", "geometry"), ("R12", "geometry_r12")):
    doc = ezdxf.new(version, setup=False)
    doc.layers.add("Walls", color=3)
    msp = doc.modelspace()
    msp.add_line((0, 0), (10, 5), dxfattribs={"layer": "Walls", "color": 1, "linetype": "CONTINUOUS"})
    msp.add_line((1, 2, 3), (4, 5, 6), dxfattribs={"thickness": 2.5, "extrusion": (0, 0, -1)})
    msp.add_point((3.25, -4.5, 9), dxfattribs={"layer": "Walls"})
    msp.add_circle((5, 5), 2.5)
    msp.add_circle((-1.5, 2, 3), 0.125, dxfattribs={"thickness": 1, "color": 2})
    msp.add_arc((0, 0), 4, 30, 120)
    msp.add_arc((2, 3, 1), 7.5, 270, 90, dxfattribs={"extrusion": (0, 0, -1), "lineweight": 50 if version != "R12" else -1})
    if version != "R12":
        msp.add_line((0, 0), (1, 1), dxfattribs={"true_color": 0xFF8000, "transparency": 0x02000040, "ltscale": 0.5, "invisible": 1})
    msp.add_solid([(0, 0), (1, 0), (0, 1), (1, 1)], dxfattribs={"thickness": 1, "extrusion": (0, 0, -1)})
    msp.add_solid([(0, 0), (1, 0), (0, 1)])
    msp.add_trace([(2, 2), (3, 2), (2, 3), (3, 3)])
    msp.add_3dface([(0, 0, 0), (1, 0, 0), (1, 1, 1), (0, 1, 1)], dxfattribs={"invisible_edges": 5})
    msp.add_3dface([(0, 0, 0), (1, 0, 0), (1, 1, 1)])
    if version != "R12":
        msp.add_ellipse((1, 2, 3), major_axis=(4, 0, 0), ratio=0.5, start_param=0.5, end_param=4.0)
        msp.add_ellipse((0, 0), major_axis=(0, 2, 0), ratio=1.0, dxfattribs={"extrusion": (0, 0, -1), "layer": "Walls"})
        msp.add_ray((1, 1, 1), (0, 1, 0))
        msp.add_xline((0, 0, 0), (1, 1, 0))
    doc.saveas(f"test/data/{name}.dxf")
