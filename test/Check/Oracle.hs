-- | The oracle: ezdxf (Python) reads the same file; @tools/compare.py@
-- compares what it found with the library's summary.
module Check.Oracle
  ( oracleFile, oracleAvailable, typedTypes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, nub)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Check.Util
import DXF

python :: FilePath
python = ".ref/venv/bin/python"

oracleAvailable :: IO Bool
oracleAvailable = doesFileExist python

-- | Compare one file with ezdxf; the differences are the failures.
oracleFile :: FilePath -> IO Result
oracleFile f = attempt f $ do
  ok <- oracleAvailable
  if not ok then return [] else do
    r <- parseFile f
    case r of
      Left e -> return [f ++ ": does not parse: " ++ show e]
      Right d -> do
        tmp <- getTemporaryDirectory
        let j = tmp </> ("dxf-summary-" ++ takeFileName f ++ ".json")
        BL.writeFile j (summaryBytes d)
        (code, out, err) <- readProcessWithExitCode python ["-I", "tools/compare.py", f, j] ""
        let ls = lines out
            bad = [l | l <- ls, any (`isPrefixOf` l) ["DIFF", "MISSING"]]
        return $ [f ++ ": " ++ l | l <- take 12 bad]
          ++ [f ++ ": compare.py: " ++ take 300 err | code /= ExitSuccess, null bad]

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
