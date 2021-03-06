module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Either
import Debug.Trace
import qualified Data.Text.Lazy.IO as IO
import Compiler
import LLVM.Pretty (ppllvm)
import System.Environment (getArgs)

import Language.Wasm.Structure

main :: IO ()
main = do
  fpath <- getArgs
  src   <- B.readFile $ head fpath
  let fname = reverse $ dropWhile (/='.') $ reverse $ head fpath
  either putStrLn (IO.writeFile (fname ++ "ll") . ppllvm) $ compile src
  where
    -- compile = parseModule >=> (\ast -> compileModule (trace (show ast) ast))
    compile = parseModule >=> compileModule
