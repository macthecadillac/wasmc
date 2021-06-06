module Common where

import qualified Data.ByteString.Lazy as B


data Interface = Interface String [Type] (Maybe Type)
data Test = T String Interface [([Val], Val)] B.ByteString

data Type = I32 | I64 | F32 | F64
  deriving (Eq)
data Val  = I Int | F Float

instance Show Type where
  show I32 = "i32"
  show I64 = "i64"
  show F32 = "f32"
  show F64 = "f64"

instance Show Val where
  show (I i) = show i
  show (F f) = show f
