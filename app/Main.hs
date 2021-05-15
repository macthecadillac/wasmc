module Main where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as IO
import Debug.Trace
import Lib
import LLVM.AST (Module)
import LLVM.Pretty
import Numeric.Natural

main :: IO ()
main = do
  fpath <- getLine
  src   <- B.readFile fpath
  either print (IO.writeFile "output.txt" . ppllvm) $ compile src
  where
    compile :: B.ByteString -> Either String Module
    compile src = compileModule =<< parseModule src
