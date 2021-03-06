{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Control.Monad.Except
import Data.Bifunctor
import Data.Binary (encode)
import Data.ByteString.Lazy (append, toStrict, ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Short.Internal (ShortByteString, toShort)
import qualified Data.List as L
import Data.Tuple (swap)
import Numeric.Natural
import System.Info (arch)

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.Name
import LLVM.AST.Type

appendIfLast :: (Show a) => (a -> Bool) -> a -> [a] -> [a]
appendIfLast f a = reverse . aux . reverse
  where
    aux []      = [a]
    aux l@(x:_) | f x       = a : l
                | otherwise = l

splitAfter :: (a -> Bool) -> [a] -> [[a]]
splitAfter f ls = filter (not . null) $ go ls []
  where
    go []    acc     = reverse acc
    go l     []      = uncurry go $ fmap pure $ swap $ break f l
    go (x:l) (hd:tl) = go l2 $ l1 : (hd ++ [x]) : tl
      where
        (l1, l2) = break f l

makeName :: String -> Natural -> Name
makeName s = mkName . (s++) . show

unsnoc :: [a] -> Maybe (a, [a])
unsnoc = fmap (second L.reverse) . L.uncons . L.reverse

operandType :: Operand -> Type
operandType (LocalReference t _) = t
operandType (ConstantOperand (Int bs _)) = IntegerType bs
operandType (ConstantOperand (Float (Single _))) = FloatingPointType FloatFP
operandType (ConstantOperand (Float (Double _))) = FloatingPointType DoubleFP
operandType t = error $ "Not a recognized type: " ++ show t

toLog :: (Show a) => String -> [a] -> [String]
toLog header []     = []
toLog header (x:xs) = (header ++ show x) : ((replicate (length header) ' ' ++) . show <$> xs)

machineNativeWordWidth :: (Integral a, MonadError String m) => m a
machineNativeWordWidth = case arch of
                           "i386"   -> pure 32
                           "x86_64" -> pure 64
                           "arm"    -> pure 32
                           "ia64"   -> pure 64
                           _        -> throwError "not a supported architecture"
