-- | The whole check: every group of checks, run in order.
module Check (run, runWith) where

import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)
import Check.Util
import Check.Oracle (oracleFiles, oracleReset)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import qualified Check.Tags
import qualified Check.Doubles
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
import qualified Check.Build

groups :: [(String, IO Result)]
groups = concat
  [ Check.Tags.checks, Check.Doubles.checks, Check.Document.checks, Check.Samples.checks
  , Check.Geometry.checks
  , Check.Text.checks
  , Check.Poly.checks
  , Check.Hatch.checks
  , Check.Dimension.checks
  , Check.Misc.checks
  , Check.Objects.checks
  , Check.Settings.checks
  , Check.Tables.checks
  , Check.Build.checks
  ]

-- | Run the checks whose name contains the given text (all if empty).
runWith :: String -> IO Bool
runWith pat = (if null pat then prefetch else oracleReset) >> runAll (filter (contains pat . fst) groups) >>= report
  where contains p s = null p || any (\i -> take (length p) (drop i s) == p) [0 .. length s]

-- | The routine check (the session's @test@): every group, but over every
-- sixth sample and f001.dxf.  @runWith ""@ is the whole check, every sample (about two minutes).
run :: IO ()
run = do
  setEnv "DXF_QUICK" "1"
  ok <- runWith ""
  unsetEnv "DXF_QUICK"
  if ok then return () else exitFailure

-- | Start with the oracle: every fixture and sample the checks will compare, several
-- Python processes at a time, so the checks find the answers ready.
prefetch :: IO ()
prefetch = do
  oracleReset
  fixtures <- map ("test/data/" ++) . filter fixture <$> listDirectory "test/data"
  samples' <- sampleFiles
  _ <- oracleFiles (fixtures ++ filter (not . isSuffixOf "3CG BASE PLANS (from the SketchUp model).dxf") samples')
  return ()
  where fixture n = ".dxf" `isSuffixOf` n && not (any (`isSuffixOf` n) [".bin.dxf", ".txt.dxf"])
