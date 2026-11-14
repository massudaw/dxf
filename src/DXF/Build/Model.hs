{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Model space as a drawing collects it: entities added one by one, held; and
-- entities made by a function of their index, never held -- each walk over the
-- model ('modelOf') makes them again, so writing a million of them holds one at a
-- time (the header, made first, needs a walk of its own for @$EXTMIN@, the layers
-- and the handle seed).
module DXF.Build.Model
  ( Piece (..), adapt, addHeld, inOrder, modelOf
  ) where

import qualified Data.ByteString as B
import Data.List (foldl')
import DXF.Build.Entities
import DXF.Build.Skeleton
import DXF.Entity
import DXF.Entity.Hatch
import DXF.Entity.Poly
import DXF.Entity.Text
import DXF.Vec

-- | A run of model space.
data Piece
  = Held ![Entity]            -- ^ added one by one (adapted; last first until 'inOrder')
  | Made !Int (Int -> Entity) -- ^ n entities, the i-th (from 0) made by the function

-- | An entity into the pieces (last first), adapted to the version.
addHeld :: Version -> Entity -> [Piece] -> [Piece]
addHeld v e ps = case ps of
  Held es : r -> let !es' = foldl' (flip (:)) es (adapt v e) in Held es' : r
  _ -> let !es' = foldl' (flip (:)) [] (adapt v e) in Held es' : ps

-- | The pieces first-added first, each held run in order: once per document, shared
-- by its walks.
inOrder :: [Piece] -> [Piece]
inOrder = foldl' (\acc p -> fix p : acc) []
  where fix (Held es) = Held (reverse es)
        fix p = p

-- | The model's entities, in order, of pieces 'inOrder'.  Every call is a walk of its
-- own: a 'Made' run is made again and not kept -- if two calls stay two.  @k@ names
-- the walk for the reader only: GHC drops it (its value is never used), and full
-- laziness or CSE then make two calls one list, held from the first walk to the last;
-- so a module that walks twice is compiled with -fno-full-laziness -fno-cse
-- ("DXF.Build" is).
modelOf :: Int -> Version -> [Piece] -> [Entity]
modelOf !_ v = go
  where
    go [] = []
    go (Held es : ps) = es ++ go ps
    go (Made n f : ps) = made 0
      where made !i | i >= n = go ps
                    | otherwise = adapt v (f i) ++ made (i + 1)
{-# NOINLINE modelOf #-}

-- | What R12 has no entity for becomes what it has: an LWPOLYLINE a POLYLINE,
-- an MTEXT one TEXT per line, a HATCH the outlines of its polygon paths.
adapt :: Version -> Entity -> [Entity]
adapt v e
  | not (isR12 v) = [e]
  | otherwise = case eBody e of
      BPoly (PLwPolyline _) -> [lwToPolyline e]
      BTxt (TMText m) ->
        [ (text l (let V3 x y z = mtInsert m in V3 x (y - fromIntegral i * mtHeight m * 1.5) z) (mtHeight m)) { eCommon = eCommon e }
        | (i, l) <- zip [0 :: Int ..] (lines' (mtextContent m)) ]
      BHatch (BHatchE (Hatch (Just h) _)) ->
        [ (polyline2d True [(p, maybe 0 id b) | (p, b) <- vs]) { eCommon = eCommon e } | PolyPath _ _ _ vs _ <- hPaths h ]
      _ -> [e]
  where
    lines' s = let (a, b) = B.breakSubstring "\\P" s in a : if B.null b then [] else lines' (B.drop 2 b)
