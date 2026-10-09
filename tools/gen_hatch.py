"""Fixtures for the hatch group: test/data/hatch.dxf (R2018) and hatch_r2000.dxf (R2000, no gradient).
Run from the project root:  .ref/venv/bin/python -I tools/gen_hatch.py
"""
import ezdxf

for version, name in (("R2018", "hatch"), ("R2000", "hatch_r2000")):
    doc = ezdxf.new(version, setup=False)
    msp = doc.modelspace()
    h = msp.add_hatch(color=3, dxfattribs={"layer": "H"})
    h.paths.add_polyline_path([(0, 0), (4, 0), (4, 3), (0, 3)], is_closed=True)
    h.paths.add_polyline_path([(1, 1, 0.5), (2, 1), (2, 2, -0.25), (1, 2)], is_closed=True, flags=16)
    h2 = msp.add_hatch(color=1)
    h2.set_pattern_fill("ANSI31", scale=2.0, angle=15)
    ep = h2.paths.add_edge_path()
    ep.add_line((0, 0), (5, 0))
    ep.add_arc((5, 1), 1, 270, 90)
    ep.add_line((5, 2), (0, 2))
    ep.add_arc((0, 1), 1, 90, 270, ccw=False)
    h2.set_seed_points([(2, 1), (3, 1)])
    h3 = msp.add_hatch()
    ep = h3.paths.add_edge_path()
    ep.add_ellipse((10, 10), (3, 0), 0.5, 0, 360)
    h4 = msp.add_hatch()
    ep = h4.paths.add_edge_path()
    ep.add_spline(control_points=[(0, 0), (1, 2), (2, 2), (3, 0)], knot_values=[0, 0, 0, 0, 1, 1, 1, 1], degree=3)
    ep.add_line((3, 0), (0, 0))
    if version == "R2018":
        h5 = msp.add_hatch()
        h5.set_gradient((255, 0, 0), (0, 0, 255), rotation=45)
        h5.paths.add_polyline_path([(0, 0), (1, 0), (1, 1)], is_closed=True)
    doc.saveas(f"test/data/{name}.dxf")
