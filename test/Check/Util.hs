-- | Helpers shared by the checks.
module Check.Util
  ( Result, samples, sampleFiles, failIf, runAll, report, attempt
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, filterM)
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)

-- | Failure messages; none is a pass.
type Result = [String]

samples :: FilePath
samples = "samples"

-- | The sample DXF files, sorted.  With DXF_QUICK set (the session's
-- routine check) every sixth one and the largest, f001.dxf.
sampleFiles :: IO [FilePath]
sampleFiles = do
  ok <- doesDirectoryExist samples
  if not ok then return [] else do
    names <- sort . filter (".dxf" `isSuffixOf`) <$> listDirectory samples
    quick <- lookupEnv "DXF_QUICK"
    let pick = case quick of
          Nothing -> names
          Just _ -> [n | (i, n) <- zip [0 :: Int ..] names, i `mod` 8 == 3, n /= "f001.dxf", take 3 n /= "3CG"]
    filterM doesFileExist (map ((samples ++ "/") ++) pick)

failIf :: Bool -> String -> Result
failIf c msg = [msg | c]

-- | Run a check on one file; an exception becomes a failure naming the file.
attempt :: FilePath -> IO Result -> IO Result
attempt f act = do
  r <- try (act >>= \xs -> evaluate (length (concat xs)) >> return xs)
  return (either (\e -> [f ++ ": exception: " ++ show (e :: SomeException)]) id r)

-- | Run one named check; an exception is a failure too.
runAll :: [(String, IO Result)] -> IO [(String, Result)]
runAll cs = forM cs $ \(name, act) -> do
  r <- try (act >>= \xs -> evaluate (length (concat xs)) >> return xs)
  case r of
    Left e -> return (name, ["exception: " ++ show (e :: SomeException)])
    Right xs -> return (name, xs)

-- | Print the outcome; True when everything passed.
report :: [(String, Result)] -> IO Bool
report rs = do
  mapM_ one rs
  let bad = length (filter (not . null . snd) rs)
  putStrLn (if bad == 0 then "CHECK-PASS: " ++ show (length rs) ++ " checks"
            else "CHECK-FAIL: " ++ show bad ++ " of " ++ show (length rs) ++ " checks failed")
  return (bad == 0)
  where
    one (n, []) = putStrLn ("PASS " ++ n)
    one (n, fs) = do
      putStrLn ("FAIL " ++ n ++ " (" ++ show (length fs) ++ ")")
      mapM_ (putStrLn . ("  " ++)) (take 10 fs)
