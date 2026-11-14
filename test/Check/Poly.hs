{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the polyline group (src/DXF/Entity/Poly.hs); fixtures from tools/gen_poly.py.
module Check.Poly (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "poly" ["test/data/poly.dxf", "test/data/poly_r12.dxf"]
  ["LWPOLYLINE", "POLYLINE", "VERTEX", "SEQEND", "INSERT", "SPLINE"]
