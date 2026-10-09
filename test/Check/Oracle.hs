-- | The oracle: ezdxf (Python) reads the same file; @tools/compare.py@
-- compares what it found with the library's summary.
module Check.Oracle
  ( oracleFile, oracleFiles, oracleReset, oracleAvailable, typedTypes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isPrefixOf, nub)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO (IOMode (..), hClose, openFile, readFile')
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Check.Util
import DXF

python :: FilePath
python = ".ref/venv/bin/python"

oracleAvailable :: IO Bool
oracleAvailable = doesFileExist python

-- | What was found for the files compared so far in this run.
cache :: IORef (M.Map FilePath Result)
cache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE cache #-}

-- | Forget it: the code or the files may have changed.
oracleReset :: IO ()
oracleReset = writeIORef cache M.empty

-- | Compare one file with ezdxf; the differences are the failures.
oracleFile :: FilePath -> IO Result
oracleFile f = oracleFiles [f]

-- | Compare files with ezdxf, six Python processes at a time (each one
-- spends a quarter of a second importing ezdxf); the differences are the
-- failures.  A file compared before in this run is not compared again.
oracleFiles :: [FilePath] -> IO Result
oracleFiles fs = do
  ok <- oracleAvailable
  if not ok then return [] else do
    known <- readIORef cache
    mapM_ batch (chunks 6 [f | f <- fs, M.notMember f known])
    known' <- readIORef cache
    return (concat [M.findWithDefault [] f known' | f <- fs])
  where
    chunks _ [] = []
    chunks n xs = let (a, b) = splitAt n xs in a : chunks n b
    batch group = do
      started <- mapM start group
      mapM_ (\st -> finish st >>= \(f, r) -> modifyIORef' cache (M.insert f r)) started
    start f = do
      r <- parseFile f
      case r of
        Left e -> return (f, Left [f ++ ": does not parse: " ++ show e])
        Right d -> do
          tmp <- getTemporaryDirectory
          let j = tmp </> ("dxf-summary-" ++ takeFileName f ++ ".json")
              o = tmp </> ("dxf-compare-" ++ takeFileName f ++ ".out")
          BL.writeFile j (summaryBytes d)
          h <- openFile o WriteMode
          (_, _, _, p) <- createProcess (proc python ["-I", "tools/compare.py", f, j]) { std_out = UseHandle h, std_err = UseHandle h }
          return (f, Right (p, h, o))
    finish (f, Left e) = return (f, e)
    finish (f, Right (p, h, o)) = do
      code <- waitForProcess p
      hClose h
      out <- readFile' o
      let bad = [l | l <- lines out, any (`isPrefixOf` l) ["DIFF", "MISSING"]]
      return (f, [f ++ ": " ++ l | l <- take 12 bad]
        ++ [f ++ ": compare.py: " ++ take 300 out | code /= ExitSuccess, null bad])

-- | The entity types a file has, typed (not kept as tags), anywhere in it.
typedTypes :: FilePath -> IO [B.ByteString]
typedTypes f = do
  r <- parseFile f
  return $ case r of
    Left _ -> []
    Right d -> nub [eType e | e <- everything d, notRaw e]
  where
    notRaw e = case eBody e of BRaw _ -> False; _ -> True
    everything d = concatMap flat (docEntities d ++ docObjects d ++ concatMap tabEntries (docTables d)
                     ++ concat [blockHead b : blockEntities b ++ maybe [] pure (blockEnd b) | b <- docBlocks d])
    flat e = e : concatMap flat (eKids e)

_unused :: B.ByteString
_unused = BC.empty
