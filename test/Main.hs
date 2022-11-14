module Main (main) where

import Test.Hspec (hspec)
import qualified TestParser

main :: IO ()
main = hspec $ do
  TestParser.test
