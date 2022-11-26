module Main (main) where

import Test.Hspec (hspec)
import qualified TestInfer
import qualified TestModuleTypeCheck
import qualified TestParser

main :: IO ()
main = hspec $ do
  TestParser.test
  TestInfer.test
  TestModuleTypeCheck.test
