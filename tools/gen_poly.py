"""Fixtures for the polyline group: test/data/poly.dxf (R2018) and poly_r12.dxf (R12).
Run from the project root:  .ref/venv/bin/python -I tools/gen_poly.py
"""
import ezdxf

for version, name in (("R2018", "poly"), ("R12", "poly_r12")):
    doc = ezdxf.new(version, setup=False)
    r12 = version == "R12"
    msp = doc.modelspace()
    if not r12:
        msp.add_lwpolyline([(0, 0), (10, 0), (10, 5), (0, 5)], close=True)
        msp.add_lwpolyline([(0, 0, 0.5, 1.0, 0.25), (3, 3, 1, 1, -0.5), (6, 0, 0, 0, 0)], format="xyseb",
                           dxfattribs={"layer": "L1", "const_width": 0.3, "elevation": 2.5, "thickness": 1.0,
                                       "extrusion": (0, 0, -1), "color": 4})
        msp.add_lwpolyline([(1, 1), (2, 2)])
    msp.add_polyline2d([(0, 0), (4, 0), (4, 4)], close=True, dxfattribs={"layer": "L1"})
    pl = msp.add_polyline2d([(0, 0, 0.5, 0.5, 0.2), (5, 5, 1, 1, 0), (9, 0, 0, 0, -1)], format="xyseb",
                            dxfattribs={"thickness": 1.5, "extrusion": (0, 0, -1)})
    msp.add_polyline3d([(0, 0, 0), (1, 2, 3), (4, 5, 6)])
    msp.add_polymesh((3, 3)).set_mesh_vertex((1, 1), (1, 1, 5))
    pf = msp.add_polyface()
    pf.append_face([(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)])
    pf.append_face([(0, 0, 1), (1, 0, 1), (1, 1, 1)])
    blk = doc.blocks.new("B1")
    blk.add_line((0, 0), (1, 1))
    msp.add_blockref("B1", (5, 5))
    msp.add_blockref("B1", (7, 8, 1), dxfattribs={"xscale": 2, "yscale": 3, "zscale": 0.5, "rotation": 45, "layer": "L1",
                                                    "extrusion": (0, 0, -1)})
    ins = msp.add_blockref("B1", (0, 0))
    ins.grid(size=(3, 2), spacing=(5, 6))
    if not r12:
        msp.add_spline([(0, 0, 0), (1, 2, 0), (3, 3, 0), (5, 0, 0)])
        s = msp.add_spline(dxfattribs={"degree": 3})
        s.control_points = [(0, 0, 0), (1, 2, 0), (3, 3, 0), (5, 0, 0), (6, 1, 0)]
        s.knots = [0, 0, 0, 0, 0.5, 1, 1, 1, 1] if False else [0, 0, 0, 0, 0.6, 1, 1, 1, 1][:9]
        s.weights = [1, 2, 1, 1, 3]
        s.dxf.start_tangent = (1, 0, 0)
        s.dxf.end_tangent = (0, 1, 0)
        msp.add_open_spline([(0, 0), (1, 1), (2, 0), (3, 1)], degree=2)
    doc.saveas(f"test/data/{name}.dxf")
