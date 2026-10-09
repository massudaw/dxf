-- | Times the stages on one file: dxf-bench FILE [+RTS -s]
module Main (main) where

import Control.Exception (evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import DXF

timed :: String -> IO a -> IO a
timed name act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  putStrLn (name ++ ": " ++ show (round ((t1 - t0) * 1000) :: Int) ++ " ms")
  return r

main :: IO ()
main = do
  [path] <- getArgs
  bs <- timed "read" (B.readFile path >>= \b -> evaluate (B.length b) >> return b)
  let (fmt, ts) = scanTags bs
  n <- timed "scan tags" (evaluate (length ts))
  putStrLn ("tags: " ++ show n)
  len <- timed "print tags" (evaluate (BL.length (BB.toLazyByteString (tTags ts fmt))))
  putStrLn ("bytes in/out: " ++ show (B.length bs) ++ " / " ++ show len)
