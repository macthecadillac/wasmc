module Main where

import Codegen (generateFile)
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Map as M
import Debug.Trace
import Instructions (MIPSFile)
import Language.Wasm.Structure
import Lib
import Numeric.Natural

main :: IO ()
main = do
  fpath <- getLine
  src   <- B.readFile fpath
  either print (writeFile "output.txt") $ compile src
  where
    compile :: B.ByteString -> Either String String
    compile src = do
      mod <- parseModule src
      let startIndex       = (\(StartFunction n) -> n) <$> start mod
          compiledCode     = fmap generateFile <$> runExceptT (compileModule (trace (show mod) mod))
          functionVarTypes = zip [0..] $ localTypes <$> functions mod
      evalState compiledCode $ Env startIndex M.empty M.empty 0 functionVarTypes
