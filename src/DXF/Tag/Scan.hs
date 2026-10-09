-- | A file's bytes to tags, whichever of the two formats it is in.
module DXF.Tag.Scan
  ( scanTags
  ) where

import qualified Data.ByteString as B
import DXF.Tag
import DXF.Tag.Binary
import DXF.Tag.Text

-- | The layout of the file and its tags.  Throws 'DxfError' (when the
-- list is consumed) on a structure error.
scanTags :: B.ByteString -> (Fmt, [Tag])
scanTags bs
  | isBinary bs = scanBinary bs
  | otherwise = scanText bs
