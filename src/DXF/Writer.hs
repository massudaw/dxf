{-# LANGUAGE FlexibleContexts,NoMonomorphismRestriction,OverloadedStrings #-}
module DXF.Writer (writeDXF, module DXF.Types) where

import DXF.Types
import Numeric
import qualified Data.Map as M

import Debug.Trace
import Control.Monad.State
import Data.Monoid
import Control.Monad
import Linear.V2
import Linear.V3
import GHC.Word
import Data.Attoparsec.Char8 as A

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Char8 as B

import qualified Data.Foldable as F
writeChar c = tell [(B.pack $ c:[] )]

writeCode c = tell [c]

writeDXF f i = writeFile f (unlines .  F.toList .snd .  runState ( dxf  i) $ Seq.empty )

tell v = do
  i <- get
  put (i <>  Seq.fromList v)


-- section :: [String] -> Writer [String] [()]
section n v = do
  mapM writeCode $ ["0","SECTION" ,"2" ,n] <> v <> ["0","ENDSEC"]

section2 n v = do
  mapM writeCode  ["0","SECTION" ,"2" ,n]
  v
  mapM writeCode ["0","ENDSEC"]

dxf (DXF h c t b e o a) = do
  section2 "HEADER"  (writeHeader h)
  section "CLASSES"  c
  section "TABLES"  t
  section2 "BLOCKS"  (mapM writeBlock b)
  section2 "ENTITIES"  (mapM writeEntity e)
  section "OBJECTS" o
  section "ACDSDATA" a
  tell ["0" , "EOF"]


icode i j = tell [i, j ]
scode i j = tell [i,show j]

writeField (n,m) = do
  icode "9" ('$': n)
  mapM (uncurry icode) $ M.toList m


writeHeader (Header h i) = do
  icode "9" "$HANDSEED"
  icode "5" (showHex i "" )
  mapM writeField (M.toList h)

writeObject (Object h p  entg gc l ente) = do
  icode "5" (showHex h "")
  icode "330" p
  icode "100" entg
  traverse (icode "67") gc
  icode "8" l
  icode "100"  ente

writeBlock (Block b n f p@(V3 ax ay az) r es o) = do
  icode "0" "BLOCK"
  writeObject b
  icode "2" n
  icode "70" f
  icode "10" (show ax)
  icode "20" (show ay)
  icode "30" (show az)
  icode "3" n
  icode "1" r
  mapM writeEnt es
  icode "0" "ENDBLK"
  writeObject o

writeEnt (Entity t ob  o ) = do
  icode "0" t
  writeObject ob
  case o of
    LWPOLYLINE b w m-> do
      scode "90" (length m)
      icode "70" (if b then "1" else "330")
      icode "43" (show w)
      mapM (\(V2 x y) -> do
        scode "10" x
        scode "20" y) m
      return ()
    LINE (V3 ax ay az) (V3 bx by bz) -> do
      scode "10" ax
      scode "20" ay
      scode "30" az
      scode "11" bx
      scode "21" by
      scode "31" bz
      return ()
    CIRCLE p@(V3 ax ay az) rz-> do
      scode "10" ax
      scode "20" ay
      scode "30" az
      scode "40" rz
      return ()
    INSERT n (V3 ax ay az) s  r -> do
      icode "2" n
      scode "10" ax
      scode "20" ay
      scode "30" az
      traverse (\(V3 sx sy sz) -> do
        scode "41" sx
        scode "42" sy
        scode "43" sz) s
      traverse (scode "50") r
      return ()
    i -> error (show i)



--writeEntity :: [Entity] -> Writer [String] [()]
writeEntity t =  writeEnt t



