{-# LANGUAGE DeriveGeneric #-}
import Test.QuickCheck
import Generic.Random
import GHC.Generics (Generic)

import qualified Language.Wasm.Structure as S
import Numeric.Natural

import Lib

main :: IO ()
main = putStrLn $ "" ++ show (countBlocks [S.I32Const 2,S.SetLocal 0,block0,S.GetLocal 1])
  where
    test = testCountBlocks [S.I32Const 2,S.SetLocal 0,block0,S.GetLocal 1] 3
    block2 = S.Block (S.Inline Nothing) [S.GetLocal 0,S.I32Eqz,S.BrIf 0,S.GetLocal 0,S.I32Const 1,S.IRelOp S.BS32 S.IEq,S.BrIf 1,S.I32Const 7,S.SetLocal 1,S.Br 2]
    block1 = S.Block (S.Inline Nothing) [block2,S.I32Const 42,S.SetLocal 1,S.Br 1]
    block0 = S.Block (S.Inline Nothing) [block1,S.I32Const 99,S.SetLocal 1]

testCountBlocks :: [S.Instruction Natural] -> Natural -> Bool
testCountBlocks l n = countBlocks l == n
