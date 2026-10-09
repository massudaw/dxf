-- | Every sample against ezdxf: what the library finds is what ezdxf finds.
module Check.Samples (checks) where

import Check.Oracle
import Check.Util

checks :: [(String, IO Result)]
checks = [("samples agree with ezdxf", sampleFiles >>= fmap concat . mapM oracleFile . filter (not . dup))]
  where dup f = f == "samples/3CG BASE PLANS (from the SketchUp model).dxf"   -- the same bytes as f001.dxf
