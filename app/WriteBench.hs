{-# LANGUAGE OverloadedStrings #-}
-- | What writing costs: dxf-write-bench N [R12|R2010] [gen] [FILE] [+RTS -s]
--
-- Builds a drawing of N lines (each on a layer, each with extended data: an
-- id string, an int, a real), turns it into a document, prints it, and says
-- the time of each stage and the size.  With FILE it prints them there as they
-- are made (hPutDocument: linbuf's sink), the two one stage.  With gen the lines
-- are not held by the drawing but made as it is walked ('addGenerated'): the
-- same bytes in the memory of one line.
module Main (main) where

import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withBinaryFile)
import DXF
import DXF.Build

timed :: String -> IO a -> IO a
timed name act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  putStrLn (name ++ ": " ++ show (fromIntegral (round ((t1 - t0) * 10000) :: Int) / 10 :: Double) ++ " ms")
  return r

drawing :: Bool -> Version -> Int -> Drawing
drawing gen v n = (if gen then addGenerated n (e . (+ 1)) else addEntities (map e [1 .. n])) (addLayer "LINES" 3 "Continuous" (addAppId "HOUSEID" (newDrawing v)))
  where
    e i = withXData (XData "HOUSEID" [XStr (BC.pack ("line-" ++ show i)), XInt (i `mod` 32000), XReal (fromIntegral i / 8)])
            (onLayer "LINES" (line (V3 (fromIntegral i) 0 0) (V3 (fromIntegral i * 1.5) (fromIntegral (i `mod` 97) * 10.25) 0)))

main :: IO ()
main = do
  (nS : args) <- getArgs
  let n = read nS
      gen = "gen" `elem` args
      rest = filter (/= "gen") args
      (ver, file) = case rest of
        ("R12" : f) -> (R12, f)
        ("R2010" : f) -> (R2010, f)
        f -> (R2010, f)
      d = drawing gen ver n
  _ <- timed "build drawing" (evaluate (length (drawingProblems d)))
  -- the sections, not the entities' spine: forced here, a million numbered entities
  -- (each a thunk on its line) would be held until printed
  doc <- timed "to document" (evaluate (let x = toDocument d in length (docSections x) `seq` x))
  size <- case file of
    [f] -> timed "print and write file" (withBinaryFile f WriteMode (\h -> hPutDocument h doc))
    _ -> timed "print" (evaluate (fromIntegral (BL.length (documentBytes doc))))
  putStrLn ("size: " ++ show size ++ " bytes, " ++ show n ++ " lines")
