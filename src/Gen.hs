{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen where

import Control.Applicative
import qualified Control.Monad.Fail as F
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import Data.Tuple
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Operand as Op
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as Type
import Numeric.Natural
import Utils (makeName, operandType, toLog)

type NestedMap a = M.Map Name.Name (M.Map Name.Name a)

data OperandStack = OpS { currentBlock :: [AST.Operand]
                        , previousBlocks :: M.Map Name.Name [AST.Operand] }
                        deriving (Show)

-- a record for per-function state variables
data InstrST = InstrST { operandStack :: M.Map Name.Name OperandStack -- stack of operands associated to a block
                       , localIdentifier :: Natural
                       , blockIdentifier :: Natural
                       , blockScopeStack :: [Name.Name]
                       , renameMap :: M.Map Name.Name Name.Name
                       , localConst :: NestedMap Constant.Constant  -- the constants associated with the identifier of the incoming block
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

newtype ModGen a = ModGen { _modGen :: ExceptT String (ReaderT ModEnv (Writer [String])) a }
  deriving (Functor, Applicative, Monad, MonadReader ModEnv, MonadWriter [String], MonadError String)

newtype InstrGen a = InstrGen { _funcGen :: StateT InstrST ModGen a }
  deriving (Functor, Applicative, Monad, MonadReader ModEnv, MonadWriter [String], F.MonadFail, MonadError String, MonadState InstrST)

instance F.MonadFail ModGen where
  fail = liftEither . Left

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a r = first (\e -> "Error: " ++ e ++ log) val
  where
    log = "\n\nLog:\n" ++ L.intercalate "\n" logLines ++ "\n<---Error"
    (val, logLines) = runWriter (runReaderT (runExceptT (_modGen a)) r)

evalInstrGen :: InstrGen a -> InstrST -> ModGen a
evalInstrGen a = evalStateT (_funcGen a)

runInstrGen :: InstrGen a -> InstrST -> ModGen (a, InstrST)
runInstrGen a = runStateT (_funcGen a)

-- choice with backtracking for stateful functions
propagateChoice :: (Alternative f, Alternative g) => (f a -> b -> g c) -> f a -> f a -> b -> g c
propagateChoice f a b s = f a s <|> f b s

instance Alternative ModGen where
  empty   = F.fail ""
  a <|> b = ModGen $ ExceptT $ reader $ propagateChoice eval a b
    where
      eval a r = fst $ runWriter (runReaderT (runExceptT (_modGen a)) r)

instance Alternative InstrGen where
  empty   = F.fail ""
  a <|> b = InstrGen $ StateT $ propagateChoice runInstrGen a b

popScope :: InstrGen ()
popScope = do
  scopeStack <- gets blockScopeStack
  (_, tail)  <- maybe (F.fail "Empty scope stack") pure $ L.uncons scopeStack
  modify $ \st -> st { blockScopeStack = tail }

pushScope :: Name.Name -> InstrGen ()
pushScope s = modify $ \st -> st { blockScopeStack = s : blockScopeStack st }

readScope :: Int -> InstrGen Name.Name
readScope i = do
  scopeStack <- gets blockScopeStack
  maybe (F.fail "Undefined scope") (pure . snd) $ L.find ((i==) . fst) $ zip [0..] scopeStack

-- seriously, consider using lens
unassignConstant :: Name.Name -> InstrGen ()
unassignConstant name = do
  st@InstrST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.delete blockID m
      f Nothing  = Nothing
  put $ st { localConst = M.alter f name localConst }

assignConstant :: Name.Name -> Constant.Constant -> InstrGen ()
assignConstant name const = do
  st@InstrST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.insert blockID const m
      f Nothing  = Just $ M.singleton blockID const
  put $ st { localConst = M.alter f name localConst }

addRenameAction :: Name.Name -> Name.Name -> InstrGen ()
addRenameAction name newName =
  modify $ \st -> st { renameMap = M.insert name newName $ renameMap st }

-- pop operand off the `operandStack`
-- the rename action here should always be valid because of SSA and because we
-- do not reuse identifiers
popOperand :: InstrGen ([AST.Named AST.Instruction], AST.Operand)
popOperand = do
  InstrST { operandStack, renameMap, blockIdentifier } <- get
  let blockID = makeName "block" blockIdentifier
  localStack <- liftMaybe $ M.lookup blockID operandStack
  (op, rest) <- liftMaybe $ uncons blockID localStack
  let operands = flip rename renameMap <$> op
  modify $ \st -> st { operandStack = M.insert blockID rest operandStack }
  (instrs, op) <- combine operands
  tell $ toLog "    pop operand--emit: " instrs
  tell $ toLog "    pop operand--op: " [op]
  pure (instrs, op)
    where
      liftMaybe = maybe (F.fail "not enough operands") pure

      uncons _ (OpS [] map)
        | null map  = Nothing
        | otherwise = do unconned <- sequenceA $ L.uncons <$> map
                         let ops  = M.toList $ fst <$> unconned
                             rest = M.filter (not . null) $ snd <$> unconned
                         pure (ops, OpS [] rest)
      uncons blockID (OpS stack map) = do
        (op, rest) <- L.uncons stack
        pure ([(blockID, op)], OpS rest map)

      rename (block, op) m = (re op m, block)
      re (Op.LocalReference t name) = Op.LocalReference t . fromMaybe name . M.lookup name
      re op                         = const op

      combine []             = F.fail "impossible branch"
      combine [(x, _)]       = pure ([], x)
      combine ops@((x, _):_) = do
        n <- gets localIdentifier
        let opType = operandType x
            ident  = makeName "tmp" n
            instr  = ident AST.:= AST.Phi opType ops []
            op     = AST.LocalReference opType ident
        incrLocalIdentifier
        pure ([instr], op)

pushOperand :: AST.Operand -> InstrGen ()
pushOperand op = do
  tell $ toLog "    push operand: " [op]
  InstrST { operandStack, blockIdentifier } <- get
  let blockID       = makeName "block" blockIdentifier
      OpS stack map = fromMaybe (OpS [] M.empty) $ M.lookup blockID operandStack
  modify $ \st -> st { operandStack = M.insert blockID (OpS (op:stack) map) operandStack }

pushOperands :: [AST.Operand] -> InstrGen ()
pushOperands = mapM_ pushOperand

deleteOperandStack :: Name.Name -> InstrGen ()
deleteOperandStack name = do
  st@InstrST { operandStack } <- get
  put $ st { operandStack = M.delete name operandStack }

branchOperandStack :: Name.Name -> Name.Name -> InstrGen [AST.Named AST.Instruction]
branchOperandStack origin dest = do
  InstrST { operandStack, localIdentifier } <- get
  let originStack  = fromMaybe (OpS [] M.empty) $ M.lookup origin operandStack
      (OpS st m)   = fromMaybe (OpS [] M.empty) $ M.lookup dest operandStack
  (n, instrs, ops) <- liftEither $ collapseStacks localIdentifier originStack
  let destStack    = OpS st $ M.insert origin ops m
  modify $ \st -> st { operandStack = M.insert dest destStack operandStack
                     , localIdentifier = n }
  pure instrs

moveOperandStack :: Name.Name -> Name.Name -> InstrGen [AST.Named AST.Instruction]
moveOperandStack origin dest = do
  instrs <- branchOperandStack origin dest
  deleteOperandStack origin
  pure instrs

collapseStacks :: Natural -> OperandStack -> Either String (Natural, [AST.Named AST.Instruction], [AST.Operand])
collapseStacks n (OpS stack map) = do
  stacks         <- transpose $ toTupleList map
  (combined, n') <- runExcept $ runStateT (traverse combine stacks) n
  let (instrs, operands) = unzip combined
  pure (n', instrs, stack ++ operands)
    where
      combine []                 = liftEither $ Left "empty sub-stack"
      combine ops@((fstOp, _):_) = do
        n <- get
        let ident  = makeName "tmp" n
            opType = operandType fstOp
            instr  = ident AST.:= AST.Phi opType ops []
            newOp  = AST.LocalReference opType ident
        modify (+1)
        pure (instr, newOp)

      transpose ls | sameLength ls = Right $ L.transpose ls
                   | otherwise     = Left "stacks of different branches have difference sizes"

      sameLength []     = True
      sameLength (x:xs) = isJust $ foldM eq (length x) $ fmap length xs
      a `eq` b | a == b    = Just b
               | otherwise = Nothing

      toTupleList map = do
        (name, ops) <- M.toList map
        pure [(op, name) | op <- ops]


incrBlockIdentifier :: InstrGen ()
incrBlockIdentifier = modify $ \st -> st { blockIdentifier = blockIdentifier st + 1 }

incrLocalIdentifier :: InstrGen ()
incrLocalIdentifier = modify $ \st -> st { localIdentifier = localIdentifier st + 1 }

-- create a new identifier, put the identifier on the `operandStack` and
-- increment the global identifier tracker
newInstructionConstructor :: Type.Type -> InstrGen (AST.Instruction -> AST.Named AST.Instruction)
newInstructionConstructor Type.VoidType = pure AST.Do
newInstructionConstructor idType        = do
  InstrST { localIdentifier, operandStack } <- get
  let name       = makeName "tmp" localIdentifier
      identifier = AST.LocalReference idType name
  pushOperand identifier
  incrLocalIdentifier
  pure (name AST.:=)

returnOperandStackItems :: InstrGen ([AST.Named AST.Instruction], AST.Named AST.Terminator)
returnOperandStackItems = do
  InstrST { operandStack, blockIdentifier, localIdentifier } <- get
  let blockID    = makeName "block" blockIdentifier
      localStack = fromMaybe (OpS [] M.empty) $ M.lookup blockID operandStack
  (n, instrs, ops) <- liftEither $ collapseStacks localIdentifier localStack
  modify $ \st -> st { localIdentifier = n }
  ret              <- collapseOps ops
  pure (instrs, AST.Do $ AST.Ret ret [])
    where
      collapseOps []  = pure Nothing
      collapseOps [x] = pure $ Just x
      collapseOps _   = F.fail "multiple ret vals not implemented"
