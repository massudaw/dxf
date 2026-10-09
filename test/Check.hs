-- | The whole check: every group of checks, run in order.
module Check (run, runWith) where

import System.Exit (exitFailure)
import Check.Util
import qualified Check.Tags
import qualified Check.Document
import qualified Check.Samples
import qualified Check.Geometry
import qualified Check.Text
import qualified Check.Poly
import qualified Check.Hatch
import qualified Check.Dimension
import qualified Check.Misc
import qualified Check.Objects
import qualified Check.Settings
import qualified Check.Tables

groups :: [(String, IO Result)]
groups = concat
  [ Check.Tags.checks, Check.Document.checks, Check.Samples.checks
  , Check.Geometry.checks
  , Check.Text.checks
  , Check.Poly.checks
  , Check.Hatch.checks
  , Check.Dimension.checks
  , Check.Misc.checks
  , Check.Objects.checks
  , Check.Settings.checks
  , Check.Tables.checks
  ]

-- | Run the checks whose name contains the given text (all if empty).
runWith :: String -> IO Bool
runWith pat = runAll (filter (contains pat . fst) groups) >>= report
  where contains p s = null p || any (\i -> take (length p) (drop i s) == p) [0 .. length s]

-- | Run everything; the process fails if a check does.
run :: IO ()
run = runWith "" >>= \ok -> if ok then return () else exitFailure
