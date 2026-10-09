{-# OPTIONS_GHC -fno-full-laziness #-}
-- | Times the stages on one file: dxf-bench FILE [doc|scan|tags] [+RTS -s]
--
--   * no mode: tag layer (retained), then the document stages
--   * @doc@:   only the document stages (parse, print, parse the print)
--   * @scan@:  only the tags, streamed and dropped (what the document parse pays for them)
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
    ["doc"] -> document bs
    ["parse"] -> do   -- parse and print only: what one document costs to hold
      d <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
      out <- timed "print document" (evaluate (BL.length (documentBytes d)))
      putStrLn ("document bytes out: " ++ show out)
    ["parseonly"] -> do
      _ <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
      pure ()
    ["loop"] -> mapM_ (\i -> evaluate (either (const 0) (const i) (parseDXF (B.copy bs)))) [1 .. 30 :: Int]   -- for a sampling profiler
    ["dbl"] -> do
      let xs = [250.24323977301532 + fromIntegral i * 1.0000123456789 | i <- [1 .. 674000 :: Int]] :: [Double]
      _ <- timed "doubleDec x 674000" (evaluate (BL.length (BB.toLazyByteString (foldMap BB.doubleDec xs))))
      _ <- timed "intDec x 674000" (evaluate (BL.length (BB.toLazyByteString (foldMap (BB.intDec . round) xs))))
      pure ()
    ["scan"] -> do
      n <- timed "scan tags (streamed)" (evaluate (length (snd (scanTags bs))))
      putStrLn ("tags: " ++ show n)
    _ -> do
      let (fmt, ts) = scanTags bs
      n <- timed "scan tags" (evaluate (length ts))
      putStrLn ("tags: " ++ show n)
      len <- timed "print tags" (evaluate (BL.length (BB.toLazyByteString (tTags ts fmt))))
      putStrLn ("bytes in/out: " ++ show (B.length bs) ++ " / " ++ show len)
      document bs

document :: B.ByteString -> IO ()
document bs = do
  d <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
  out <- timed "print document" (evaluate (BL.length (documentBytes d)))
  putStrLn ("document bytes out: " ++ show out)
  let printed = BL.toStrict (documentBytes d)
  _ <- evaluate (B.length printed)
  d2 <- timed "parse printed" (evaluate (parseDXF printed) >>= either (ioError . userError . show) pure)
  putStrLn ("stable: " ++ show (d == d2))
