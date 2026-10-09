-- | The whole check: every group of checks, run in order.
module Check (run, runWith) where

import System.Exit (exitFailure)
import Check.Util
import qualified Check.Tags
import qualified Check.Document

groups :: [(String, IO Result)]
groups = Check.Tags.checks ++ Check.Document.checks

-- | Run the checks whose name contains the given text (all if empty).
runWith :: String -> IO Bool
runWith pat = runAll (filter (contains pat . fst) groups) >>= report
  where contains p s = null p || any (\i -> take (length p) (drop i s) == p) [0 .. length s]

-- | Run everything; the process fails if a check does.
run :: IO ()
run = runWith "" >>= \ok -> if ok then return () else exitFailure
