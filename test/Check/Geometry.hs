{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the geometry group (src/DXF/Entity/Geometry.hs).
module Check.Geometry (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "geometry" ["test/data/geometry.dxf", "test/data/geometry_r12.dxf"] ["LINE", "POINT", "CIRCLE", "ARC", "ELLIPSE", "SOLID", "TRACE", "3DFACE", "RAY", "XLINE"]
