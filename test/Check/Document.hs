{-# LANGUAGE OverloadedStrings #-}
-- | Whole files: every sample parses, prints, and parses again to the
-- same value; nothing the file held is lost on the way.
module Check.Document
  ( checks, roundTripFile, missingTags
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Check.Util
import DXF

-- | A tag's identity for comparison: numbers by value.
norm :: Tag -> (Int, Double, B.ByteString)
norm (Tag c v) = case kindOf c of
  KStr -> (c, 0, v)
  KBin -> (c, 0, v)
  KDouble -> maybe (c, 0, v) (\d -> (c, d, B.empty)) (readDouble v)
  _ -> maybe (c, 0, v) (\n -> (c, fromIntegral n, B.empty)) (readInt v)

-- | Tags of the first list that the second does not hold (as a multiset).
missingTags :: [Tag] -> [Tag] -> [Tag]
missingTags orig new
  | length orig == length new && and (zipWith (\a b -> norm a == norm b) orig new) = []   -- the usual case: same tags, same order
  | otherwise = go orig have
  where
    have = M.fromListWith (+) [(norm t, 1 :: Int) | t <- new]
    go [] _ = []
    go (t : ts) m = case M.lookup (norm t) m of
      Just n | n > 0 -> go ts (M.insert (norm t) (n - 1) m)
      _ -> t : go ts m

-- | Without the entry counts of table heads, which are repaired when too small.
stripCounts :: [Tag] -> [Tag]
stripCounts (t@(Tag 0 "TABLE") : r) = let (hd, rest) = break (\(Tag c _) -> c == 0) r
                                       in t : filter (\(Tag c _) -> c /= 70 && c /= 71) hd ++ stripCounts rest
stripCounts (t : r) = t : stripCounts r
stripCounts [] = []

-- | Parse, print, parse again; the failures.
roundTripFile :: FilePath -> IO Result
roundTripFile f = attempt f $ do
  bs <- B.readFile f
  case parseDXF bs of
    Left e -> return [f ++ ": does not parse: " ++ show e]
    Right d1 -> do
      let out1 = BL.toStrict (documentBytes d1)
      case parseDXF out1 of
        Left e -> return [f ++ ": printed file does not parse: " ++ show e]
        Right d2 -> do
          let out2 = BL.toStrict (documentBytes d2)
              (_, t0) = scanTags bs
              (_, t1) = scanTags out1
              lost = missingTags (stripCounts (filter (\(Tag c _) -> c /= 999) t0)) (stripCounts t1)
          return $ concat
            [ failIf (d1 /= d2) (f ++ ": parse (print d) /= d")
            , failIf (out1 /= out2) (f ++ ": print is not stable")
            , [f ++ ": lost tag " ++ show t | t <- take 5 lost]
            , failIf (length lost > 5) (f ++ ": ... " ++ show (length lost) ++ " lost tags in all") ]

allSamples :: IO Result
allSamples = sampleFiles >>= fmap concat . mapM roundTripFile

checks :: [(String, IO Result)]
checks = [("document: parse, print, parse again, nothing lost (every sample)", allSamples)]
