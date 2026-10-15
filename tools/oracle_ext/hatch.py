"""Parts of a hatch ezdxf does not show as DXF attributes."""


def hatch(e):
    paths = []
    for p in e.paths:
        if type(p).__name__ == "PolylinePath":
            paths.append(["polyline", len(p.vertices)])
        else:
            paths.append(["edges", len(p.edges)])
    return {
        "paths": paths,
        "pattern_lines": len(e.pattern.lines) if e.pattern else 0,
        "seeds": [list(s[:2]) for s in e.seeds],
    }


EXT = {"HATCH": hatch}
