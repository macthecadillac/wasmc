{-# LANGUAGE DeriveGeneric #-}
import Test.QuickCheck
import Generic.Random
import GHC.Generics (Generic)

import qualified Language.Wasm.Structure as S
import Numeric.Natural

import Lib

main :: IO ()
main = pure ()

testCountBlocks :: [S.Instruction Natural] -> Natural -> Bool
testCountBlocks l n = undefined -- countBlocks l == n
