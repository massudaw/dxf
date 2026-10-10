{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | The pieces fast decoders are made of: a reader of tags straight from the
-- bytes of a text file, as a small state monad over a byte offset.  A
-- reader that meets anything but the shape it expects fails, and the
-- entity goes the general way ("DXF.Codec"), which takes every shape.
--
-- Each primitive reads what the codec field of the same name reads:
-- 'mdbl' is @mdbl@, 'pt' is @pt3@ (a missing z is 0), and so on.  The
-- optional ones consume nothing when the next tag is another code.
module DXF.Entity.FastKit
  ( R, Env (..), runR
  , commonR, marks, end, peek
  , dbl, mdbl, int, intD, mint, str, strI, mstrI, pt, ptD, mpt
  ) where

import qualified Data.ByteString as B
import DXF.Entity.Common
import DXF.Intern
import DXF.Number
import DXF.Tag.Text
import DXF.Vec

data Env = Env { envBytes :: !B.ByteString, envTable :: !Interner }

data Res a = No | Ok !a {-# UNPACK #-} !Int

newtype R a = R (Env -> Int -> Res a)

unR :: R a -> Env -> Int -> Res a
unR (R f) = f
{-# INLINE unR #-}

instance Functor R where
  fmap f (R g) = R (\e p -> case g e p of No -> No; Ok a q -> Ok (f a) q)
  {-# INLINE fmap #-}

instance Applicative R where
  pure a = R (\_ p -> Ok a p)
  R f <*> R g = R (\e p -> case f e p of
    No -> No
    Ok h q -> case g e q of No -> No; Ok a r -> Ok (h a) r)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad R where
  R g >>= f = R (\e p -> case g e p of No -> No; Ok a q -> unR (f a) e q)
  {-# INLINE (>>=) #-}

-- | Run a reader at an offset: the result and the offset after it.
runR :: Interner -> B.ByteString -> Int -> R a -> Maybe (a, Int)
runR tbl bs p (R f) = case f (Env bs tbl) p of
  No -> Nothing
  Ok a q -> Just (a, q)
{-# INLINE runR #-}

-- | The entity is over: the next tag is a @0@.
end :: R ()
end = R (\(Env bs _) p -> case nextTag bs p of
  T 0 _ _ _ -> Ok () p
  _ -> No)

-- | The code of the next tag, which stays to be read.
peek :: R Int
peek = R (\(Env bs _) p -> case nextTag bs p of T c _ _ _ -> Ok c p)

-- | Skip the subclass markers with these names (any number of them).
marks :: [B.ByteString] -> R ()
marks names = R go
  where
    go env@(Env bs _) p = case nextTag bs p of
      T 100 s e nx | slice bs s e `elem` names -> go env nx
      _ -> Ok () p

-- | A required double.
dbl :: Int -> R Double
dbl c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c, Just d <- readDouble (slice bs s e) -> Ok d nx
  _ -> No)

-- | An optional double.
mdbl :: Int -> R (Maybe Double)
mdbl c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> case readDouble (slice bs s e) of
                             Just d -> Ok (Just d) nx
                             Nothing -> No
  _ -> Ok Nothing p)

-- | A required integer.
int :: Int -> R Int
int c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c, Just d <- readInt (slice bs s e) -> Ok d nx
  _ -> No)

-- | An integer with a default.
intD :: Int -> Int -> R Int
intD def c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> case readInt (slice bs s e) of
                             Just d -> Ok d nx
                             Nothing -> No
  _ -> Ok def p)

-- | An optional integer.
mint :: Int -> R (Maybe Int)
mint c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> case readInt (slice bs s e) of
                             Just d -> Ok (Just d) nx
                             Nothing -> No
  _ -> Ok Nothing p)

-- | A required string, as the slice of the input it is.
str :: Int -> R B.ByteString
str c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> Ok (slice bs s e) nx
  _ -> No)

-- | A required string, interned.
strI :: Int -> R B.ByteString
strI c = R (\(Env bs t) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> Ok (intern t (slice bs s e)) nx
  _ -> No)

-- | An optional string, interned.
mstrI :: Int -> R (Maybe B.ByteString)
mstrI c = R (\(Env bs t) p -> case nextTag bs p of
  T c1 s e nx | c1 == c -> Ok (Just (intern t (slice bs s e))) nx
  _ -> Ok Nothing p)

-- | A point: x at code c, y at c+10, z at c+20 when it is there.
pt :: Int -> R V3
pt c = R (\(Env bs _) p -> point bs c p)

-- | A point that may be absent (the default is the origin).
ptD :: Int -> R V3
ptD c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 _ _ _ | c1 == c -> point bs c p
  _ -> Ok v3zero p)

-- | An optional point.
mpt :: Int -> R (Maybe V3)
mpt c = R (\(Env bs _) p -> case nextTag bs p of
  T c1 _ _ _ | c1 == c -> case point bs c p of
                            Ok v q -> Ok (Just v) q
                            No -> No
  _ -> Ok Nothing p)

point :: B.ByteString -> Int -> Int -> Res V3
point bs c p0 = case nextTag bs p0 of
  T c1 s1 e1 n1 | c1 == c, Just x <- readDouble (slice bs s1 e1) -> case nextTag bs n1 of
    T c2 s2 e2 n2 | c2 == c + 10, Just y <- readDouble (slice bs s2 e2) -> case nextTag bs n2 of
      T c3 s3 e3 n3 | c3 == c + 20 -> case readDouble (slice bs s3 e3) of
                        Just z -> Ok (V3 x y z) n3
                        Nothing -> No
      _ -> Ok (V3 x y 0) n2
    _ -> No
  _ -> No
{-# INLINE point #-}

-- | The common part: handle (5), owner (330), @AcDbEntity@ (100), layer
-- (8), linetype (6), color (62), paperspace (67), lineweight (370), each
-- once, in any order.  Layer, linetype and owner are interned.
commonR :: R Common
commonR = R (\(Env bs tbl) p0 ->
  let share v = Just $! intern tbl v
      go !ent h o l lt col ps lw !p = case nextTag bs p of
        T c s e nx
          | c == 5, not ent, Nothing <- h -> go ent (Just v) o l lt col ps lw nx
          | c == 330, not ent, Nothing <- o -> go ent h (share v) l lt col ps lw nx
          | c == 100, not ent, v == "AcDbEntity" -> go True h o l lt col ps lw nx
          | c == 8, Nothing <- l -> go ent h o (share v) lt col ps lw nx
          | c == 6, Nothing <- lt -> go ent h o l (share v) col ps lw nx
          | c == 62, Nothing <- col, Just k <- readInt v -> go ent h o l lt (Just k) ps lw nx
          | c == 67, Nothing <- ps, Just k <- readInt v -> go ent h o l lt col (Just k) lw nx
          | c == 370, Nothing <- lw, Just k <- readInt v -> go ent h o l lt col ps (Just k) nx
          | otherwise -> Ok (Common h o [] l lt col ps lw Nothing blankMore [] []) p
          where !v = slice bs s e
  in go False Nothing Nothing Nothing Nothing Nothing Nothing Nothing p0)
