"""Fixtures for the dimension group: test/data/dimension.dxf (R2018) and dimension_r12.dxf (R12, no LEADER).
Run from the project root:  .ref/venv/bin/python -I tools/gen_dimension.py
"""
import ezdxf

for version, name in (("R2018", "dimension"), ("R12", "dimension_r12")):
    doc = ezdxf.new(version, setup=True)
    msp = doc.modelspace()
    msp.add_linear_dim(base=(3, 2), p1=(0, 0), p2=(6, 0)).render()
    msp.add_linear_dim(base=(0, 3), p1=(0, 0), p2=(0, 5), angle=90, text="<> mm").render()
    msp.add_aligned_dim(p1=(0, 0), p2=(5, 5), distance=1).render()
    msp.add_angular_dim_2l(base=(2, 2), line1=((0, 0), (4, 0)), line2=((0, 0), (0, 4))).render()
    msp.add_angular_dim_3p(base=(2, 2), center=(0, 0), p1=(3, 0), p2=(0, 3)).render()
    msp.add_diameter_dim(center=(0, 0), radius=3, angle=30).render()
    msp.add_radius_dim(center=(0, 0), radius=3, angle=60).render()
    msp.add_ordinate_dim(feature_location=(2, 3), offset=(1, 1), dtype=0, origin=(0, 0)).render() if hasattr(msp, "add_ordinate_dim") else None
    if version != "R12":
        msp.add_leader([(0, 0), (1, 1), (2, 1)], dxfattribs={"layer": "L"})
    doc.saveas(f"test/data/{name}.dxf")
