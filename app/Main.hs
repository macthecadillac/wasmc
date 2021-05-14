module Main where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as IO
import Debug.Trace
import Language.Wasm.Structure
import Lib
import LLVM.Pretty
import Numeric.Natural

main :: IO ()
main = IO.writeFile "output.ll" $ ppllvm module_
-- main = do
--   fpath <- getLine
--   src   <- B.readFile fpath
--   either print (writeFile "output.txt") $ compile src
--   where
--     compile :: B.ByteString -> Either String String
--     compile src = do
--       mod <- parseModule src
--       let startIndex       = (\(StartFunction n) -> n) <$> start mod
--           compiledCode     = fmap generateFile <$> runExceptT (compileModule (trace (show mod) mod))
--           functionVarTypes = zip [0..] $ localTypes <$> functions mod
--       evalState compiledCode $ Env startIndex M.empty M.empty 0 functionVarTypes
