{-# LANGUAGE OverloadedStrings #-}
-- | Checks of the objects group (src/DXF/Entity/Objects.hs); fixtures from tools/gen_objects.py.
module Check.Objects (checks) where

import Check.Group
import Check.Util

checks :: [(String, IO Result)]
checks = groupChecks "objects" ["test/data/objects.dxf"]
  ["DICTIONARY", "ACDBDICTIONARYWDFLT", "DICTIONARYVAR", "XRECORD", "GROUP", "IMAGEDEF", "IMAGEDEF_REACTOR", "RASTERVARIABLES", "SORTENTSTABLE", "ACDBPLACEHOLDER"]
