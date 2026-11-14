{-# LANGUAGE OverloadedStrings #-}
-- | A change of the real printer, held against the one before it: each file printed by the previous printer
-- (a directory of its outputs, by file name) against this one's print of the same file.  Every tag that
-- differs is a real, and its new text reads back to the bit-identical double of its old text; the file this
-- printer writes reads back to the original's model.
module Check.Reals
  ( FileReals (..), realsOf, realsReport
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM)
import GHC.Float (castDoubleToWord64)
import System.FilePath (takeFileName, (</>))
import DXF

data FileReals = FileReals
  { frFile :: FilePath
  , frReals :: Int                          -- ^ real tags in the new print
  , frChanged :: [(Int, B.ByteString, B.ByteString)]   -- ^ (code, old text, new text)
  , frProblems :: [String] }

-- | One file: the old print (from the directory) against the new.
realsOf :: FilePath -> FilePath -> IO FileReals
realsOf oldDir f = do
  orig <- B.readFile f
  old <- B.readFile (oldDir </> takeFileName f)
  case parseDXF orig of
    Left e -> return (FileReals f 0 [] ["does not parse: " ++ show e])
    Right d0 -> do
      let new = BL.toStrict (documentBytes d0)
          (_, to) = scanTags old
          (_, tn) = scanTags new
          changed = [(c, a, b) | (Tag c a, Tag _ b) <- zip to tn, a /= b]
          bits = fmap castDoubleToWord64 . readDouble
          wrong = [ show c ++ ": " ++ BC.unpack a ++ " -> " ++ BC.unpack b
                  | (c, a, b) <- changed, kindOf c /= KDouble || bits b == Nothing || bits a /= bits b ]
          model = case parseDXF new of
            Left e -> ["the new print does not parse: " ++ show e]
            Right d1 -> ["the new print reads back to another model" | d1 /= d0]
      return FileReals
        { frFile = f
        , frReals = length [() | Tag c _ <- tn, kindOf c == KDouble]
        , frChanged = changed
        , frProblems = concat
            [ ["tag codes differ" | length to /= length tn || or (zipWith (\(Tag a _) (Tag b _) -> a /= b) to tn)]
            , take 5 wrong, model ] }

-- | Every file: a line each, the totals, a few changed reals.
realsReport :: FilePath -> [FilePath] -> IO Bool
realsReport oldDir fs = do
  rs <- forM fs $ \f -> do
    r <- realsOf oldDir f
    putStrLn (takeFileName f ++ ": " ++ show (length (frChanged r)) ++ " of " ++ show (frReals r) ++ " reals changed"
              ++ concatMap ("; " ++) (frProblems r))
    return r
  let total = sum (map frReals rs)
      ch = concatMap frChanged rs
      bad = filter (not . null . frProblems) rs
  putStrLn ("TOTAL: " ++ show (length ch) ++ " of " ++ show total ++ " reals changed, in "
            ++ show (length (filter (not . null . frChanged) rs)) ++ " of " ++ show (length rs) ++ " files; "
            ++ show (length bad) ++ " files with problems")
  putStrLn ("shorter: " ++ show (length [() | (_, a, b) <- ch, B.length b < B.length a])
            ++ ", same length: " ++ show (length [() | (_, a, b) <- ch, B.length b == B.length a])
            ++ ", longer: " ++ show (length [() | (_, a, b) <- ch, B.length b > B.length a]))
  mapM_ (\(c, a, b) -> putStrLn ("  " ++ show c ++ ": " ++ BC.unpack a ++ " -> " ++ BC.unpack b)) (take 12 ch)
  return (null bad)
