module Utils where

import Data.Binary (encode)
import Data.ByteString.Lazy (append, toStrict, ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Short.Internal (ShortByteString, toShort)
import Data.Tuple (swap)
import LLVM.AST.Name
import Numeric.Natural

splitAfter :: (a -> Bool) -> [a] -> [[a]]
splitAfter f ls = filter (not . null) $ go ls []
  where
    go []    acc     = reverse acc
    go l     []      = uncurry go $ fmap pure $ swap $ break f l
    go (x:l) (hd:tl) = go l2 $ l1 : (hd ++ [x]) : tl
      where
        (l1, l2) = break f l

makeName :: ByteString -> Natural -> Name
makeName s = Name . toShort . toStrict . append s . pack . show
