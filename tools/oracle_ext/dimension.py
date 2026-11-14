"""Parts of a leader ezdxf does not show as DXF attributes."""


def leader(e):
    return {"vertices": [list(v) for v in e.vertices]}


EXT = {"LEADER": leader}
