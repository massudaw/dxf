"""Parts of the polyline group ezdxf does not show as DXF attributes."""


def lw(e):
    return {"points": [list(p) for p in e.get_points("xyseb")]}


def spline(e):
    return {
        "knots": list(e.knots),
        "weights": list(e.weights),
        "control_points": [list(p) for p in e.control_points],
        "fit_points": [list(p) for p in e.fit_points],
    }


EXT = {"LWPOLYLINE": lw, "SPLINE": spline}
