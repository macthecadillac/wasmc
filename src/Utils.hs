module Utils where

import Data.Bifunctor (second)
import Data.Tuple (swap)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f ls = go ls []
  where
    go []    acc     = reverse acc
    go l     []      = uncurry go $ second pure $ swap $ break f l
    go (x:l) (hd:tl) = go l2 $ l1 : (hd ++ [x]) : tl
      where
        (l1, l2) = break f l