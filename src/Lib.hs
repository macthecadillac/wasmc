module Lib
    ( parseModule
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Language.Wasm.Parser as P
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Lexer as L

parseModule :: B.ByteString -> Either String S.Module
parseModule bs = L.scanner bs >>= P.parseModule
