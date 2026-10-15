"""Fixtures for the objects and settings groups: test/data/objects.dxf (R2018).
Run from the project root:  .ref/venv/bin/python -I tools/gen_objects.py
"""
import ezdxf

doc = ezdxf.new("R2018", setup=True)
msp = doc.modelspace()
d = doc.rootdict.add_new_dict("MY_DICT")
d.add_dict_var("VAR1", "hello")
d.add_dict_var("VAR2", "x" * 10)
xr = d.add_xrecord("XREC")
xr.reset([(1, "text"), (40, 3.5), (70, 7), (10, (1.0, 2.0, 3.0)), (330, "1F"), (280, 4)])
g = doc.groups.new("MYGROUP")
g.extend([msp.add_line((0, 0), (1, 1)), msp.add_circle((0, 0), 1)])
idef = doc.add_image_def("a.png", size_in_pixel=(10, 20))
msp.add_image(idef, (0, 0), (1, 1))
doc.layouts.new("Second")
ls = doc.mline_styles.new("MYSTYLE")
ls.elements.append(0.5, 3, "DASHED") if hasattr(ls.elements, "append") else None
doc.objects.add_dxf_object_with_reactor("PLOTSETTINGS", {"owner": doc.rootdict.dxf.handle, "paper_size": "A4", "plot_layout_flags": 688}) if hasattr(doc.objects, "add_dxf_object_with_reactor") else None
msp.set_redraw_order([(msp[0].dxf.handle, 5), (msp[1].dxf.handle, 1)])
doc.saveas("test/data/objects.dxf")
