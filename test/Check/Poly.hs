{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the poly group (owner: whoever owns src/DXF/Entity/Poly.hs).
module Check.Poly (checks) where

import Check.Group
import Check.Util

-- | Fixtures come from tools/gen_poly.py.
checks :: [(String, IO Result)]
checks = []  -- groupChecks "poly" ["test/data/poly.dxf", "test/data/poly_r12.dxf"] ["TYPE", ...]
