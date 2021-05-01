module Main where

import Lib
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = mod >>= (print . show)
  where
    mod = parseModule <$> (getLine >>= L.readFile)
