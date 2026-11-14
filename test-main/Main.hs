-- | The test driver (as portkit's bench/port/common/Main.hs, so the check can be timed as a program):
--   (no argument)  Check.run -- the routine check, as the session's `test`
--   routine        the same
--   full           Check.runWith "" -- every check on every fixture and sample
--   with TEXT      Check.runWith TEXT
module Main (main) where

import qualified Check
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = getArgs >>= \as -> case as of
  [] -> Check.run
  ["routine"] -> Check.run
  ["full"] -> Check.runWith "" >>= done
  ["with", p] -> Check.runWith p >>= done
  _ -> putStrLn "TEST [full | with TEXT]" >> exitFailure
  where done ok = if ok then pure () else exitFailure
