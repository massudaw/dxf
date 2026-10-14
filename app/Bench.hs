{-# OPTIONS_GHC -fno-full-laziness #-}
-- | Times the stages on one file: dxf-bench FILE [doc|scan|tags] [+RTS -s]
--
--   * no mode: tag layer (retained), then the document stages
--   * @doc@:   only the document stages (parse, print, parse the print)
--   * @scan@:  only the tags, streamed and dropped (what the document parse pays for them)
module Main (main) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Set as Set
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (ptrToIntPtr)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStr, stderr)
import MemBudget.Registry (report)
import DXF
import DXF.Number.Lemire (eiselLemire)

timed :: String -> IO a -> IO a
timed name act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  putStrLn (name ++ ": " ++ show (fromIntegral (round ((t1 - t0) * 10000) :: Int) / 10 :: Double) ++ " ms")
  return r

ptrAddr :: ForeignPtr a -> Int
ptrAddr fp = fromIntegral (ptrToIntPtr (unsafeForeignPtrToPtr fp))

main :: IO ()
main = body >> mbReport

-- | membudget's report of every table, on stderr, when @MB_REPORT@ is set (the output untouched).
mbReport :: IO ()
mbReport = lookupEnv "MB_REPORT" >>= mapM_ (const (report >>= hPutStr stderr))

body :: IO ()
body = do
  (path : mode) <- getArgs
  bs <- timed "read" (B.readFile path >>= \b -> evaluate (B.length b) >> return b)
  case mode of
    ["doc"] -> document bs
    ["parse"] -> do   -- parse and print only: what one document costs to hold
      d <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
      out <- timed "print document" (evaluate (BL.length (documentBytes d)))
      putStrLn ("document bytes out: " ++ show out)
    ["stages"] -> do   -- scan alone, then each section (sections are lazy: forced one at a time)
      _ <- timed "scan tags (streamed)" (evaluate (length (snd (scanTags bs))))
      let secs = docSections (parseDocument bs)
      let go k = do   -- forcing the k-th cell of the list is what parses section k
            n <- timed ("section " ++ show k) (evaluate (rnf (take k secs)) >> evaluate (length (take k secs)))
            if n < k then pure () else do
              putStrLn ("  = " ++ map (toEnum . fromEnum) (B.unpack (sectionName (secs !! (k - 1)))))
              go (k + 1)
      go (1 :: Int)
      _ <- evaluate (rnf secs)
      pure ()
    ["census"] -> do   -- what a parsed document holds: strings by value and by place in memory
      d <- either (ioError . userError . show) pure (parseDXF bs)
      let es = docEntities d ++ concatMap (\b -> blockHead b : blockEntities b) (docBlocks d)
          fields = [ ("type", map (Just . eType) es), ("layer", map (cLayer . eCommon) es)
                   , ("linetype", map (cLinetype . eCommon) es), ("owner", map (cOwner . eCommon) es)
                   , ("handle", map (cHandle . eCommon) es) ]
          place v = let BI.BS fp n = v in (fp `seq` ptrAddr fp, n)
      putStrLn ("entities: " ++ show (length es))
      mapM_ (\(name, vs) -> do
               let js = [v | Just v <- vs]
               putStrLn (name ++ ": " ++ show (length js) ++ " held, " ++ show (Set.size (Set.fromList js))
                         ++ " distinct values, " ++ show (Set.size (Set.fromList (map place js))) ++ " distinct in memory")) fields
    ["table"] -> do   -- one line: size, cold parse/print (first run), steady parse/print (min of a loop)
      let sz = B.length bs
          ms t = fromIntegral (round (t * 10000) :: Int) / 10 :: Double
          mbs t = fromIntegral (round (fromIntegral sz / t / 1e5) :: Int) / 10 :: Double
          clock act = do { t0 <- getMonotonicTime; r <- act; t1 <- getMonotonicTime; pure (r, t1 - t0) }
          parseOf b = evaluate (parseDXF b) >>= either (ioError . userError . show) pure
      (d, cp) <- clock (parseOf bs)
      (_, cw) <- clock (evaluate (BL.length (documentBytes d)))
      let n = max 5 (min 300 (round (1.0 / max 0.0005 cp))) :: Int
      sp <- fmap minimum (mapM (\i -> do { b <- evaluate (B.copy bs); snd <$> clock (parseOf b >>= \x -> evaluate (rnf x) >> pure i) }) [1 .. n])
      sw <- fmap minimum (mapM (\i -> snd <$> clock (evaluate (BL.length (documentBytes d) + fromIntegral i))) [1 .. n])
      putStrLn (path ++ ": size " ++ show sz ++ " | cold parse " ++ show (ms cp) ++ " ms " ++ show (mbs cp) ++ " MB/s, print "
                ++ show (ms cw) ++ " ms " ++ show (mbs cw) ++ " MB/s | loop(" ++ show n ++ ") parse " ++ show (ms sp) ++ " ms "
                ++ show (mbs sp) ++ " MB/s, print " ++ show (ms sw) ++ " ms " ++ show (mbs sw) ++ " MB/s")
    ["round", nS, out] -> do   -- N parses (each of a fresh copy) and N prints; the last print written to OUT
      let n = read nS :: Int
          parseOf b = either (ioError . userError . show) pure (parseDXF b)
          once = evaluate (B.copy bs) >>= parseOf >>= \d -> evaluate (rnf d) >> pure d
      mapM_ (const once) [2 .. n]
      d <- once
      mapM_ (\i -> evaluate (BL.length (documentBytes d) + fromIntegral i)) [2 .. n]
      writeDocument out d
    ["parseonly"] -> do
      _ <- timed "parse document" (evaluate (parseDXF bs) >>= either (ioError . userError . show) pure)
      pure ()
    ["loop"] -> mapM_ (\i -> evaluate (either (const 0) (const i) (parseDXF (B.copy bs)))) [1 .. 400 :: Int]   -- for a sampling profiler
    ["printloop"] -> case parseDXF bs of   -- for a sampling profiler
      Right d -> mapM_ (\i -> evaluate (BL.length (documentBytes d) + i)) [1 .. 30]
      Left e -> ioError (userError (show e))
    ["init"] -> do   -- the one-time tables of the number code
      _ <- timed "first eiselLemire (builds its table)" (evaluate (eiselLemire 17882 (-3)))
      _ <- timed "first doubleText (builds its table)" (evaluate (BL.length (BB.toLazyByteString (doubleText 2.5))))
      pure ()
    ["dloop"] -> do   -- for a sampling profiler
      let xs = [250.24323977301532 + fromIntegral i * 1.0000123456789 | i <- [1 .. 674000 :: Int]] :: [Double]
      _ <- evaluate (length xs)
      mapM_ (\k -> evaluate (BL.length (BB.toLazyByteString (foldMap (\x -> tD 10 (x + fromIntegral k) (Fmt False False False False)) xs)))) [1 .. 40 :: Int]
    ["dbl"] -> do
      let xs = [250.24323977301532 + fromIntegral i * 1.0000123456789 | i <- [1 .. 674000 :: Int]] :: [Double]
      _ <- timed "doubleDec x 674000" (evaluate (BL.length (BB.toLazyByteString (foldMap BB.doubleDec xs))))
      _ <- timed "doubleText x 674000" (evaluate (BL.length (BB.toLazyByteString (foldMap (\x -> tD 10 x (Fmt False False False False)) xs))))
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
