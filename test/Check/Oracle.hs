-- | The oracle on portkit (portkit's bench/port/dxf, request 2f21c0f8): ezdxf, through tools/compare.py, against
-- what this library found in a file (DXF.Summary's JSON).  "PortKit.Oracle" keeps compare.py's answer on disk
-- (.ghci-session/oracle), keyed by the drawing, the summary of ours and the oracle's version (compare.py, its
-- extensions, ezdxf's version); before, it was kept in memory only, so each test run asked ezdxf about every
-- file again.
module Check.Oracle
  ( oracleFile, oracleFiles, oracleReset, oracleAvailable, typedTypes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isPrefixOf, nub, sort)
import System.Directory (doesFileExist, getTemporaryDirectory, listDirectory)
import System.FilePath (takeFileName, (</>))
import Check.Util
import DXF
import PortKit.Oracle

python :: FilePath
python = ".ref/venv/bin/python"

oracleAvailable :: IO Bool
oracleAvailable = doesFileExist python

-- | compare.py FILE SUMMARY, its standard output and error together and its exit status as a last line (1 is
-- "differences found", the lines say which; anything else, or 1 with no line, is compare.py failing).
ezdxf :: FilePath -> Oracle
ezdxf tmp = (oracle (\_ f _ -> Prints "/bin/sh" ["-c", "\"$0\" -I tools/compare.py \"$1\" \"$2\" 2>&1; echo \"EXIT $?\"", python, f, summaryOf tmp f]))
  { oracleVersion = do
      ext <- sort . filter (".py" `isSuffixOfS`) <$> listDirectory ("tools" </> "oracle_ext")
      fs <- versionFiles ("tools/compare.py" : map (("tools" </> "oracle_ext") </>) ext)
      v <- versionCommand python ["-I", "-c", "import ezdxf; print(ezdxf.__version__)"]
      pure (fs ++ v)
  , oracleExitOk = const True }
  where isSuffixOfS s x = reverse s `isPrefixOf` reverse x

summaryOf :: FilePath -> FilePath -> FilePath
summaryOf tmp f = tmp </> ("dxf-summary-" ++ takeFileName f ++ ".json")

-- | What each file came to, for the session (dxf's own rule: oracleReset empties it).
cache :: IORef (M.Map FilePath Result)
cache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE cache #-}

oracleReset :: IO ()
oracleReset = writeIORef cache M.empty

oracleFile :: FilePath -> IO Result
oracleFile f = oracleFiles [f]

-- | Every file's differences from ezdxf: six at a time, as dxf's own did.
oracleFiles :: [FilePath] -> IO Result
oracleFiles fs = do
  ok <- oracleAvailable
  if not ok then return [] else do
    known <- readIORef cache
    tmp <- getTemporaryDirectory
    -- six at a time, parsed, summarised and compared before the next six are parsed: a drawing parsed is let go
    -- before the next batch (all parsed first held 90 drawings at once)
    mapM_ (batch tmp) (chunks 6 (nub [ f | f <- fs, M.notMember f known ]))
    known' <- readIORef cache
    return (concat [ M.findWithDefault [] f known' | f <- fs ])
  where
    chunks _ [] = []
    chunks n xs = let (a, b) = splitAt n xs in a : chunks n b
    batch tmp group = do
      parsed <- mapM (\f -> parseFile f >>= \r -> case r of
                          Left e -> pure (f, Just [f ++ ": does not parse: " ++ show e])
                          Right d -> BL.writeFile (summaryOf tmp f) (summaryBytes d) >> pure (f, Nothing)) group
      let jobs = [ f | (f, Nothing) <- parsed ]
      kept <- oracleMany 6 (ezdxf tmp) [ ("compare", f, [summaryOf tmp f]) | f <- jobs ]
      rs <- mapM (verdict tmp) (zip jobs kept)
      modifyIORef' cache (M.union (M.fromList (rs ++ [ (f, e) | (f, Just e) <- parsed ])))
    verdict tmp (f, k) = case k of
      Left e -> pure (f, [f ++ ": compare.py: " ++ take 300 e])
      Right kk -> withKeptText (ezdxf tmp) "compare" f kk (\t -> let out = BC.unpack (BL.toStrict t) in length out `seq` pure out)
        >>= \r -> pure (f, either (\e -> [f ++ ": compare.py: " ++ take 300 e]) (lines' f) r)
    lines' f out =
      let ls = lines out
          code = case reverse ls of l : _ | "EXIT " `isPrefixOf` l -> drop 5 l; _ -> "?"
          body = unlines (filter (not . ("EXIT " `isPrefixOf`)) ls)
          bad = [ l | l <- lines body, any (`isPrefixOf` l) ["DIFF", "MISSING"] ]
          -- the answer is about this drawing (not another's kept answer in its place: portkit's canary, bb6b78c2)
          named = [ l | l <- ls, "DRAWING " `isPrefixOf` l ] == ["DRAWING " ++ f]
      in [ f ++ ": compare.py answered about another drawing (" ++ take 200 (unwords [ l | l <- ls, "DRAWING " `isPrefixOf` l ]) ++ ")" | not named ]
         ++ [ f ++ ": " ++ l | l <- take 12 bad ] ++ [ f ++ ": compare.py: " ++ take 300 body | code /= "0", null bad ]

-- | The entity types a file has typed (not kept as tags only).
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
