module Main where

import qualified Parser (parse)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Parser.parse
