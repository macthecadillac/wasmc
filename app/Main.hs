module Main where

import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Text.Lazy.IO as IO
import Lib
import LLVM.Pretty (ppllvm)
import System.Environment (getArgs)

main :: IO ()
main = do
  fpath <- getArgs
  src   <- B.readFile $ head fpath
  let fname = reverse $ dropWhile (not . (=='.')) $ reverse $ head fpath
  either print (IO.writeFile (fname ++ "ll") . ppllvm) $ compile src
  where
    compile src = compileModule =<< parseModule src
