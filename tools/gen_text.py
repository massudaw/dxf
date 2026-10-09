"""Fixtures for the text group: test/data/text.dxf (R2018) and text_r12.dxf (R12).
Run from the project root:  .ref/venv/bin/python -I tools/gen_text.py
"""
import ezdxf

for version, name in (("R2018", "text"), ("R12", "text_r12")):
    doc = ezdxf.new(version, setup=False)
    r12 = version == "R12"
    msp = doc.modelspace()
    msp.add_text("plain", dxfattribs={"insert": (1, 2, 0), "height": 3.5})
    msp.add_text("full attribs", dxfattribs={
        "insert": (1, 2, 3), "height": 2, "rotation": 30, "width": 0.8, "oblique": 15, "style": "Standard",
        "text_generation_flag": 2, "halign": 4, "valign": 1, "align_point": (5, 6, 7), "layer": "L1", "color": 3,
        "thickness": 1.5, "extrusion": (0, 0, -1)})
    msp.add_text("aligned", dxfattribs={"insert": (0, 0), "height": 1, "halign": 1, "valign": 2, "align_point": (3, 3)})
    msp.add_text("Grüße é", dxfattribs={"insert": (0, 0), "height": 1})
    blk = doc.blocks.new("WITHATTR")
    blk.add_attdef("TAG1", (0, 0), "default", dxfattribs={"height": 1, "prompt": "Enter", "flags": 1})
    blk.add_attdef("TAG2", (0, 2), "d2", dxfattribs={"height": 1, "rotation": 45, "lock_position": 1} if not r12 else {"height": 1})
    ins = msp.add_blockref("WITHATTR", (10, 10))
    ins.add_attrib("TAG1", "value one", (10, 10), dxfattribs={"height": 1, "layer": "L1"})
    ins.add_attrib("TAG2", "value two", (10, 12), dxfattribs={"height": 1, "rotation": 10})
    if not r12:
        msp.add_mtext("short")
        long = "A long paragraph. " * 40 + "\\P" + "second line with {\\fArial;bold} text. " * 10
        msp.add_mtext(long, dxfattribs={
            "insert": (5, 5, 1), "char_height": 2.5, "width": 80, "attachment_point": 5, "style": "Standard",
            "rotation": 20, "line_spacing_factor": 1.5, "layer": "L1", "extrusion": (0, 0, -1)})
        m = msp.add_mtext("with background", dxfattribs={"insert": (0, 0), "char_height": 1})
        m.set_bg_color(2)
        m2 = msp.add_mtext("true color bg", dxfattribs={"insert": (0, 5), "char_height": 1})
        m2.set_bg_color((255, 128, 0), scale=2.0)
        msp.add_mtext("direction", dxfattribs={"insert": (0, 0), "char_height": 1, "text_direction": (0, 1, 0)})
        msp.new_entity("TOLERANCE", {"insert": (1, 1, 0), "content": "{\\Fgdt;r}%%v0.5%%vA", "dimstyle": "Standard"})
    shape_style = doc.styles.new("MYSHAPES", dxfattribs={"font": "my.shx", "flags": 1}) if not r12 else None
    msp.new_entity("SHAPE", {"insert": (2, 3, 0), "size": 4, "name": "TRI", "rotation": 5, "xscale": 1.2, "oblique": 3})
    doc.saveas(f"test/data/{name}.dxf")
