{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen where

import Control.Applicative ((<|>), Alternative)
import qualified Control.Monad.Fail as F
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Operand as Op
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as Type
import Numeric.Natural
import Utils (makeName)

-- a record for per-function state variables
data FuncST = FuncST { operandStack :: [AST.Operand]
                     , localIdentifier :: Natural
                     , blockIdentifier :: Natural
                     , blockScopeStack :: [Name.Name]
                     , renameMap :: M.Map Name.Name Name.Name
                     , localConst :: M.Map Name.Name (M.Map Name.Name Constant.Constant)  -- the constants associated with the identifier of the incoming block
                     , localVariableTypes :: M.Map Name.Name Type.Type }
                     deriving (Show)

data FunctionType = FT { arguments :: [Type.Type], returnType :: Type.Type }
  deriving (Show)

-- a record for per-module constants
data ModEnv = ModEnv { startFunctionIndex :: Maybe Natural
                     , functionTypes :: M.Map Natural FunctionType
                    --  , globalVariableTypes :: M.Map Natural Type.Type
                      }
                     deriving (Show)

newtype ModGen a = ModGen { runModGen :: ExceptT String (Reader ModEnv) a }
  deriving (Functor, Applicative, MonadPlus, Alternative, Monad, MonadReader ModEnv, MonadError String)

newtype FuncGen a = FuncGen { runFuncGen :: StateT FuncST ModGen a }
  deriving (Functor, Applicative, MonadPlus, Alternative, Monad, MonadReader ModEnv, F.MonadFail, MonadError String, MonadState FuncST)

instance F.MonadFail ModGen where
  fail = liftEither . Left

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a = runReader (runExceptT (runModGen a))

evalFuncGen :: FuncGen a -> FuncST -> ModGen a
evalFuncGen a = evalStateT (runFuncGen a)

popScope :: FuncGen ()
popScope = do
  scopeStack <- blockScopeStack <$> get
  (_, tail)  <- maybe (error "Empty scope stack") pure $ L.uncons scopeStack
  modify (\st -> st { blockScopeStack = tail })

pushScope :: Name.Name -> FuncGen ()
pushScope s = modify (\st -> st { blockScopeStack = s : blockScopeStack st })

readScope :: Int -> FuncGen Name.Name
readScope i = do
  scopeStack <- blockScopeStack <$> get
  maybe (error "Undefined scope") (pure . snd) $ L.find ((i==) . fst) $ zip [0..] scopeStack

-- seriously, consider using lens
unassignConstant :: Name.Name -> FuncGen ()
unassignConstant name = do
  st@FuncST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.delete blockID m
      f Nothing  = Nothing
  put $ st { localConst = M.alter f name localConst }

assignConstant :: Name.Name -> Constant.Constant -> FuncGen ()
assignConstant name const = do
  st@FuncST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.insert blockID const m
      f Nothing  = Just $ M.singleton blockID const
  put $ st { localConst = M.alter f name localConst }

addRenameAction :: Name.Name -> Name.Name -> FuncGen ()
addRenameAction name newName =
  modify (\st -> st { renameMap = M.insert name newName $ renameMap st })

-- pop operand off the `operandStack`
-- the rename action here should always be valid because of SSA and because we
-- do not reuse identifiers
popOperand :: FuncGen AST.Operand
popOperand = do
  stack      <- operandStack <$> get
  (op, rest) <- maybe (F.fail "not enough operands") pure $ L.uncons stack
  operand    <- rename op <$> get
  modify (\st -> st { operandStack = rest })
  pure operand
    where
      rename (Op.LocalReference t name) = Op.LocalReference t . fromMaybe name . M.lookup name . renameMap
      rename op                         = const op

pushOperand :: AST.Operand -> FuncGen ()
pushOperand op = modify (\st -> st { operandStack = op : operandStack st })

pushOperands :: [AST.Operand] -> FuncGen ()
pushOperands = mapM_ pushOperand

incrLocalIdentifier :: FuncGen ()
incrLocalIdentifier = modify (\st -> st { localIdentifier = localIdentifier st + 1 })

-- create a new identifier, put the identifier on the `operandStack` and
-- increment the global identifier tracker
newInstructionConstructor :: Type.Type -> FuncGen (AST.Instruction -> AST.Named AST.Instruction)
newInstructionConstructor Type.VoidType = pure AST.Do
newInstructionConstructor idType        = do
  FuncST { localIdentifier, operandStack } <- get
  let name       = makeName "tmp" localIdentifier
      identifier = AST.LocalReference idType name
  pushOperand identifier
  incrLocalIdentifier
  pure (name AST.:=)

returnOperandStackItems :: FuncGen (AST.Named AST.Terminator)
returnOperandStackItems = do
  FuncST { operandStack } <- get
  let ret = do guard $ not $ L.null operandStack
               pure $ packValues operandStack
  modify (\env -> env { operandStack = [] })
  pure $ AST.Do $ AST.Ret ret []
    where
      packValues []  = AST.LocalReference Type.VoidType "void"
      packValues [x] = x
      packValues l   = error "add support for multiple return vals"  -- TODO: support multiple return values
