{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
                        , previousBlocks :: (M.Map Name.Name [AST.Operand]) }
                        deriving (Show)

-- a record for per-function state variables
data FuncST = FuncST { operandStack :: M.Map Name.Name OperandStack -- stack of operands associated to a block
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

newtype FuncGen a = FuncGen { _funcGen :: StateT FuncST ModGen a }
  deriving (Functor, Applicative, Monad, MonadReader ModEnv, MonadWriter [String], F.MonadFail, MonadError String, MonadState FuncST)

instance F.MonadFail ModGen where
  fail = liftEither . Left

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a r = first (\e -> "Error: " ++ e ++ log) val
  where
    log = "\n\nLog:\n" ++ (L.concat $ L.intersperse "\n" logLines) ++ "\n<---Error"
    (val, logLines) = runWriter (runReaderT (runExceptT (_modGen a)) r)

evalFuncGen :: FuncGen a -> FuncST -> ModGen a
evalFuncGen a = evalStateT (_funcGen a)

runFuncGen :: FuncGen a -> FuncST -> ModGen (a, FuncST)
runFuncGen a = runStateT (_funcGen a)

-- choice with backtracking for stateful functions
propagateChoice :: (Alternative f, Alternative g) => (f a -> b -> g c) -> f a -> f a -> b -> g c
propagateChoice f a b s = (f a s) <|> (f b s)

instance Alternative ModGen where
  empty   = F.fail ""
  a <|> b = ModGen $ ExceptT $ reader $ propagateChoice eval a b
    where
      eval a r = fst $ runWriter (runReaderT (runExceptT (_modGen a)) r)

instance Alternative FuncGen where
  empty   = F.fail ""
  a <|> b = FuncGen $ StateT $ propagateChoice runFuncGen a b

popScope :: FuncGen ()
popScope = do
  scopeStack <- blockScopeStack <$> get
  (_, tail)  <- maybe (F.fail "Empty scope stack") pure $ L.uncons scopeStack
  modify $ \st -> st { blockScopeStack = tail }

pushScope :: Name.Name -> FuncGen ()
pushScope s = modify $ \st -> st { blockScopeStack = s : blockScopeStack st }

readScope :: Int -> FuncGen Name.Name
readScope i = do
  scopeStack <- blockScopeStack <$> get
  maybe (F.fail "Undefined scope") (pure . snd) $ L.find ((i==) . fst) $ zip [0..] scopeStack

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
  modify $ \st -> st { renameMap = M.insert name newName $ renameMap st }

-- pop operand off the `operandStack`
-- the rename action here should always be valid because of SSA and because we
-- do not reuse identifiers
popOperand :: FuncGen ([AST.Named AST.Instruction], AST.Operand)
popOperand = do
  FuncST { operandStack, renameMap, blockIdentifier } <- get
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

      combine []             = error "impossible branch"
      combine [(x, _)]       = pure ([], x)
      combine ops@((x, _):_) = do
        n <- localIdentifier <$> get
        let opType = operandType x
            ident  = makeName "tmp" n
            instr  = ident AST.:= AST.Phi opType ops []
            op     = AST.LocalReference opType ident
        incrLocalIdentifier
        pure ([instr], op)

pushOperand :: AST.Operand -> FuncGen ()
pushOperand op = do
  tell $ toLog "    push operand: " [op]
  FuncST { operandStack, blockIdentifier } <- get
  let blockID       = makeName "block" blockIdentifier
      OpS stack map = fromMaybe (OpS [] M.empty) $ M.lookup blockID operandStack
  modify $ \st -> st { operandStack = M.insert blockID (OpS (op:stack) map) operandStack }

pushOperands :: [AST.Operand] -> FuncGen ()
pushOperands = mapM_ pushOperand

deleteOperandStack :: Name.Name -> FuncGen ()
deleteOperandStack name = do
  st@FuncST { operandStack } <- get
  put $ st { operandStack = M.delete name operandStack }

branchOperandStack :: Name.Name -> Name.Name -> FuncGen ([AST.Named AST.Instruction])
branchOperandStack origin dest = do
  FuncST { operandStack, localIdentifier } <- get
  let originStack  = fromMaybe (OpS [] M.empty) $ M.lookup origin operandStack
      (OpS st m)   = fromMaybe (OpS [] M.empty) $ M.lookup dest operandStack
  (n, instrs, ops) <- liftEither $ collapseStacks localIdentifier originStack
  let destStack    = OpS st $ M.insert origin ops m
  modify $ \st -> st { operandStack = M.insert dest destStack operandStack
                     , localIdentifier = n }
  pure instrs

moveOperandStack :: Name.Name -> Name.Name -> FuncGen ([AST.Named AST.Instruction])
moveOperandStack origin dest = do
  instrs <- branchOperandStack origin dest
  deleteOperandStack origin
  pure instrs

collapseStacks :: Natural -> OperandStack -> Either String (Natural, [AST.Named AST.Instruction], [AST.Operand])
collapseStacks n (OpS stack map) = do
  stacks <- transpose $ toTupleList map
  let (combined, n')     = runState (traverse combine $ stacks) n
      (instrs, operands) = unzip combined
  pure (n', instrs, stack ++ operands)
    where
      combine []                 = error "empty sub-stack"
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


incrBlockIdentifier :: FuncGen ()
incrBlockIdentifier = modify $ \st -> st { blockIdentifier = blockIdentifier st + 1 }

incrLocalIdentifier :: FuncGen ()
incrLocalIdentifier = modify $ \st -> st { localIdentifier = localIdentifier st + 1 }

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

returnOperandStackItems :: FuncGen ([AST.Named AST.Instruction], AST.Named AST.Terminator)
returnOperandStackItems = do
  FuncST { operandStack, blockIdentifier, localIdentifier } <- get
  let blockID    = makeName "block" blockIdentifier
      localStack = fromMaybe (OpS [] M.empty) $ M.lookup blockID operandStack
  (n, instrs, ops) <- liftEither $ collapseStacks localIdentifier localStack
  modify $ \st -> st { localIdentifier = n }
  pure (instrs, AST.Do $ AST.Ret (collapseOps ops) [])
    where
      collapseOps []  = Nothing
      collapseOps [x] = Just x
      collapseOps _   = error "multiple ret vals not implemented"
