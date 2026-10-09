{-# LANGUAGE OverloadedStrings #-}
-- | The checks every group of entity types has: its fixture files (DXF
-- written by ezdxf from @tools/gen_GROUP.py@, in @test/data@) parse, print
-- and parse again, lose no tag, agree with ezdxf, and hold every type of
-- the group as a typed entity.
module Check.Group
  ( groupChecks
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Check.Document (roundTripFile)
import Check.Oracle
import Check.Util

-- | @groupChecks name fixtures types@.
groupChecks :: String -> [FilePath] -> [B.ByteString] -> [(String, IO Result)]
groupChecks name files types =
  [ (name ++ ": fixtures round trip, nothing lost", concat <$> mapM roundTripFile files)
  , (name ++ ": fixtures agree with ezdxf", concat <$> mapM oracleFile files)
  , (name ++ ": every type is typed in the fixtures (" ++ BC.unpack (BC.unwords types) ++ ")", covered)
  ]
  where
    covered = do
      found <- concat <$> mapM typedTypes files
      return [name ++ ": no typed " ++ BC.unpack t ++ " in the fixtures" | t <- types, t `notElem` found]
