{-# LANGUAGE TupleSections,NoMonomorphismRestriction,OverloadedStrings #-}
module DXF.Parser (readDXF,module DXF.Types  ) where

import DXF.Types
import Control.Applicative
import qualified Data.Map as M

import Debug.Trace
import Data.Monoid
import Numeric
import Control.Monad
import Linear.V2
import Linear.V3
import GHC.Word
import Data.Attoparsec.Char8 as A
import Data.Attoparsec as A

import qualified Data.ByteString.Char8 as B


readCode' c = manyTill (char8 ' ') c >> endOfLine
readCode c = manyTill anyChar (string c)  >> endOfLine

main = do
  fmap entities<$> readDXF "PATH.DXF"
readDXF f =   B.readFile f >>= return .   parseOnly dxf


section :: B.ByteString  -> Parser [String]
section n = do
  readCode' "0"  >> string "SECTION" >> endOfLine >> readCode "2" >> string n >> endOfLine
  manyTill line  (readCode' "0" >> string "ENDSEC" >> endOfLine)

dxf = do
  h <- section "HEADER" >>= parseHeaderVars
  c <- section "CLASSES" >>= parseHeader
  t <- section "TABLES" >>= parseHeader
  b <- section "BLOCKS" >>= parseBlocks
  e <- section "ENTITIES" >>= parseEntity
  o <- section "OBJECTS" >>= parseHeader
  a <- section "ACDSDATA" >>= parseHeader
  return  $ DXF h c t b e o a

line = manyTill A.anyChar endOfLine
icode i = readCode i >> line
icode' i = readCode' i >> line
rcode' i = readCode' i >> read <$> line

v3Code =  do
    ax <- rcode' "10"
    ay <- rcode' "20"
    az <- rcode' "30"
    return $ V3 ax ay az

extrusion =  do
    v <-liftA3 V3 <$>  mcode "210" <*> mcode "220" <*> mcode "230"
    return  (fmap read <$> v )
parseEnt = do
  t <- icode "0"
  o <- parseObject
  Entity t o <$> case t of
    "TEXT" -> do
      ax <-  icode' "10"
      ay <- icode' "20"
      az <- icode' "30"
      h <- icode' "40"
      t <- icode' "1"
      r <- mcode "50"
      aix <- mcode "210"
      aiy <- mcode "220"
      aiz <- mcode "230"
      icode "100"
      return (TEXT  (read <$> V3 ax ay az) (read h)  t (read  <$> r) (fmap read <$> liftA3 V3 aix aiy aiz))
    "LWPOLYLINE" -> do
      n <- icode' "90"
      b <- icode' "70"
      w <- icode' "43"
      th <- fmap read <$>  mcode "38"
      m <- replicateM (read n) (do
        x <- rcode' "10"
        y <- rcode' "20"
        w <- fmap read <$> mcode "42"
        return (V2 x  y ,w))
      r <- mcode "50"
      v <-  extrusion
      return (LWPOLYLINE (if b /= "1" then True else False) (read w) th  m (read <$> r) v)
    "LINE" -> do
      ax <-icode "10"
      ay <-icode "20"
      az <- icode "30"
      bx <- icode "11"
      by <- icode "21"
      bz <- icode "31"
      return (LINE (read <$> V3 ax ay az) (read <$> V3 bx by bz))
    "ATTRIB" -> do

      ax <- icode' "10"
      ay <- icode' "20"
      az <- icode' "30"
      rz <- icode' "40"
      v <- icode' "1"
      rw <- mcode "41"
      sn <- mcode "7"
      scm <- icode' "100"
      scm2 <- mcode "280"
      v3 <- mcode "2"
      l <- mcode "70"
      v2 <- mcode "280"
      return (ATTRIB (read <$> V3 ax ay az) (read rz) (v) (read <$> rw) (sn) scm scm2 v3 (read <$> l) v2 )
    "CIRCLE" -> do
      ax <- icode' "10"
      ay <- icode' "20"
      az <- icode' "30"
      rz <- icode' "40"
      return (CIRCLE  (read <$> V3 ax ay az) (read rz))
    "POLYLINE" -> do
      mf <- mcode "66"
      v <- v3Code
      f <- rcode' "70"
      l <- manyTill parseEnt (readCode' "0" >> string "SEQEND" >> endOfLine)
      o <- parseObject
      return $ POLYLINE v f (l <> [Entity "SEQEND" o SEQEND])
    "VERTEX" -> do
      t <- icode' "100"
      v <- v3Code
      f <- rcode' "70"
      return $ VERTEX t v f
    "SEQEND" -> return SEQEND
    "INSERT" -> do
      mf <- mcode "66"
      let ins =  do
            n <- icode' "2"
            ax <- icode' "10"
            ay <- icode' "20"
            az <- icode' "30"
            sx <- mcode "41"
            sy <- mcode "42"
            sz <- mcode "43"
            r <- mcode "50"
            aix <- mcode "210"
            aiy <- mcode "220"
            aiz <- mcode "230"
            return (INSERT n (read <$> V3 ax ay az) (fmap read <$> liftA3 V3 sx sy sz) (read  <$> r) (fmap read <$> liftA3 V3 aix aiy aiz))
      case read <$>  mf of
        Just 1 -> do
             v <- ins
             l <- manyTill parseEnt (readCode' "0" >> string "SEQEND" >> endOfLine)
             o <- parseObject
             return $ v (l <> [Entity "SEQEND" o SEQEND])
        Just 0 -> do
              v <- ins
              return (v [])
        Nothing -> do
              v <- ins
              return (v [])

    "SEQEND" -> return SEQEND
    i -> error i

mcode i =   (Just <$> icode' i) <|> (return Nothing)


parseEntity :: [String] -> Parser [Entity]
parseEntity t= do
  case parseOnly  (many1  parseEnt) (B.pack $ unlines t ) of
    Right i -> return i
    Left  i -> error (i <> show t)

readVar = do
  char '$'
  h <- line
  (h,) . M.fromList <$> manyTill ((,) <$> line <*> line) (readCode' "9")
rerun t m = case parseOnly m (B.pack $ unlines t) of
              Right r -> return r
              Left e -> error e

parseObject  = do
  h <- icode "5"
  p <- icode "330"
  entg <- icode "100"
  gc <- mcode "67"
  l <- icode "8"
  lt <- mcode "6"
  bc <- mcode "62"
  lw <- mcode "370"
  eps <- mcode "48"
  ente <- mcode "100"
  return (Object (fst $ head$  readHex h) p entg  gc l lt bc lw eps ente)



parseBlocks t = rerun t $ do
  many' (do
    readCode' "0"
    string "BLOCK"
    o <-parseObject
    n <- icode "2"
    f <- icode "70"
    ax <- icode "10"
    ay <- icode "20"
    az <- icode "30"
    n <- icode "3"
    r <- icode "1"
    es <- manyTill parseEnt (readCode' "0" >> string "ENDBLK")
    eo <- parseObject
    return $ Block  o  n f (read <$> V3 ax ay az) r es eo)


parseHeaderVars t= do
  case parseOnly (do
            readCode' "9"
            m <- M.fromList <$> many' readVar
            let seed = case fmap M.toList $ M.lookup "HANDSEED" m of
                        Just [(s,v)] -> case parseOnly  hexadecimal (B.pack v) of
                                          Right v -> v
                                          Left e -> error e
            return (Header  (M.delete "HANDSEED" m) seed )
            ) (B.pack $ unlines t) of
      Right i -> return i
      Left i -> error (i <> show t)

parseHeader t= do
  return t


