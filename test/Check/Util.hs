-- | Helpers shared by the checks: the protocol (results, report) is "PortKit.Check"'s; the samples are dxf's.
module Check.Util
  ( Result, samples, sampleFiles, failIf, runAll, report, attempt
  ) where

import Control.Monad (filterM)
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import PortKit.Check (Result, attempt, failIf, isQuick, report, runAll)

samples :: FilePath
samples = "samples"

-- | The sample drawings, sorted; under DXF_QUICK one in eight (the session's routine check).
sampleFiles :: IO [FilePath]
sampleFiles = do
  ok <- doesDirectoryExist samples
  if not ok then return [] else do
    names <- sort . filter (".dxf" `isSuffixOf`) <$> listDirectory samples
    quick <- isQuick "DXF_QUICK"
    let pick = if not quick then names
               else [n | (i, n) <- zip [0 :: Int ..] names, i `mod` 8 == 3, n /= "f001.dxf", take 3 n /= "3CG"]
    filterM doesFileExist (map ((samples ++ "/") ++) pick)
