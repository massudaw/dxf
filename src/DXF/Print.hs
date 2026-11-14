{-# LANGUAGE LinearTypes, OverloadedStrings #-}
-- | A 'Document' to bytes.  Written in the layout it was read in (text or
-- binary, line ending, padded group codes); subclass markers are left out
-- when the header says R12.
module DXF.Print
  ( printDocument, writeDocument, hPutDocument, documentBytes, sectionOut
  ) where

import System.IO (Handle, IOMode (WriteMode), withBinaryFile)
import LinBuf.Builder (builder)
import LinBuf.Write (closeSink, withSink)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import DXF.Classes
import DXF.Document
import DXF.Entity
import DXF.Header
import DXF.Tables
import DXF.Tag
import DXF.Tag.Binary (binarySignature)
import DXF.Tag.Out

printDocument :: Document -> BB.Builder
printDocument d = (if fmtBinary fmt then BB.byteString binarySignature else mempty) <> body fmt
  where
    fmt = (docFmt d) { fmtR12 = headerIsR12 (docHeader d) }
    body = (if fmtBinary fmt then mempty else tTags (docPreamble d))
      <> foldMap sectionOut (docSections d) <> tS 0 "EOF"

documentBytes :: Document -> BL.ByteString
documentBytes = BB.toLazyByteString . printDocument

-- | Write a document to a file as it is printed (through linbuf's sink): neither the
-- printed chunks nor a copy of them are held.
writeDocument :: FilePath -> Document -> IO ()
writeDocument p d = withBinaryFile p WriteMode (\h -> () <$ hPutDocument h d)

-- | Print a document to a handle, the Builder's steps run straight in a sink of 256 KB
-- flushed to the handle as it fills; the bytes written.
hPutDocument :: Handle -> Document -> IO Int
hPutDocument h d = withSink h (256 * 1024) (\buf -> closeSink (builder (printDocument d) buf))

wrap :: B.ByteString -> Out -> Out
wrap name o = tS 0 "SECTION" <> tS 2 name <> o <> tS 0 "ENDSEC"

sectionOut :: Section -> Out
sectionOut s = wrap (sectionName s) $ case s of
  SHeader h -> headerOut h
  SClasses cs -> foldMap classOut cs
  STables ts -> foldMap tableOut ts
  SBlocks bs -> foldMap blockOut bs
  SEntities es -> foldMap encodeEntity es
  SObjects es -> foldMap encodeEntity es
  SThumbnail ts -> tTags ts
  SOther _ ts -> tTags ts

blockOut :: Block -> Out
blockOut (Block h es e) = encodeEntity h <> foldMap encodeEntity es <> maybe mempty encodeEntity e
