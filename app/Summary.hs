-- | dxf-summary FILE: what the library found, as JSON (for tools/compare.py).
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import DXF

main :: IO ()
main = do
  [path] <- getArgs
  r <- parseFile path
  case r of
    Left e -> hPutStrLn stderr (show e) >> exitFailure
    Right d -> BL.putStr (summaryBytes d)
