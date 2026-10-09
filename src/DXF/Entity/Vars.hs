{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | Records that are a name, flags, and a long list of variables told
-- apart by their group code (DIMSTYLE has seventy, VPORT and VIEW a few
-- dozen).  The variables are kept as the tags they were, in order, and
-- typed through a table of names, codes and arity -- the names are
-- ezdxf's.
module DXF.Entity.Vars
  ( Vars (..), VarDef, varsCodec
  , varStr, varInt, varDouble, varPoint
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out

data Vars = Vars
  { vName  :: !B.ByteString        -- ^ 2
  , vFlags :: !(Maybe Int)         -- ^ the first 70
  , vVars  :: [(Int, B.ByteString)] -- ^ all other tags the table knows, in file order (a second 70 among them)
  } deriving (Eq, Show, Generic, NFData)

-- | A variable's ezdxf name, its group code, and how many coordinates it
-- has (1 for a scalar, 2 or 3 for a point at codes c, c+10, c+20).
type VarDef = (B.ByteString, Int, Int)

varStr :: Int -> Vars -> Maybe B.ByteString
varStr c v = lookup c (vVars v)

varInt :: Int -> Vars -> Maybe Int
varInt c v = varStr c v >>= readInt

varDouble :: Int -> Vars -> Maybe Double
varDouble c v = varStr c v >>= readDouble

-- | The coordinates of a point variable at x code c.
varPoint :: Int -> Int -> Vars -> Maybe [Double]
varPoint n c v = mapM (\k -> varDouble (c + 10 * k) v) [0 .. n - 1]

-- | The codec for a table whose record subclass marker is given.
varsCodec :: B.ByteString -> [VarDef] -> Codec Vars
varsCodec marker defs = codec (Vars "" Nothing []) $
  [ mark "AcDbSymbolTableRecord", mark marker
  , str "name" 2 (lens vName (\r v -> r { vName = v }))
  , custom "flags" (70 : codes) rd (\r -> maybe mempty (tI 70) (vFlags r) <> foldMap (\(c, v) -> tRaw (Tag c v)) (vVars r))
      (fmap JI . vFlags) (\r -> r { vVars = reverse (vVars r) }) ]
  ++ map dumpOnly defs
  where
    codes = [c + 10 * k | (_, c, n) <- defs, k <- [0 .. n - 1], not (c == 70 && k == 0)]
    rd 70 v r = readInt v >>= \n -> Just (case vFlags r of
                  Nothing -> r { vFlags = Just n }
                  Just _ -> r { vVars = (70, v) : vVars r })
    rd c v r = Just r { vVars = (c, v) : vVars r }
    dumpOnly (n, c, k) = custom n [] (\_ _ _ -> Nothing) (const mempty) (dump k c) id
    dump 1 c r = jv c <$> lookupVar c r
    dump k c r = JA <$> mapM (\i -> jv (c + 10 * i) <$> lookupVar (c + 10 * i) r) [0 .. k - 1]
    lookupVar c r = if c == 70 then lookup 70 (vVars r) else lookup c (vVars r)
    jv c v = case kindOf c of
      KDouble -> maybe (JS v) JD (readDouble v)
      KStr -> JS v
      KBin -> JS v
      _ -> maybe (JS v) JI (readInt v)
