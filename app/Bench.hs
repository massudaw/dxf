{-# OPTIONS_GHC -fno-full-laziness #-}
-- | Times the stages on one file: dxf-bench FILE [doc] [+RTS -s]  (doc: only the document stages)
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
  (path : mode) <- getArgs
  bs <- timed "read" (B.readFile path >>= \b -> evaluate (B.length b) >> return b)
  case mode of
    ["doc"] -> pure ()
    _ -> do
      let (fmt, ts) = scanTags bs
      n <- timed "scan tags" (evaluate (length ts))
      putStrLn ("tags: " ++ show n)
      len <- timed "print tags" (evaluate (BL.length (BB.toLazyByteString (tTags ts fmt))))
      putStrLn ("bytes in/out: " ++ show (B.length bs) ++ " / " ++ show len)
  d <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
  out <- timed "print document" (evaluate (BL.length (documentBytes d)))
  putStrLn ("document bytes out: " ++ show out)
  let printed = BL.toStrict (documentBytes d)
  _ <- evaluate (B.length printed)
  d2 <- timed "parse printed" (evaluate (parseDXF printed) >>= either (ioError . userError . show) pure)
  putStrLn ("stable: " ++ show (d == d2))
