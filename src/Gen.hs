{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.List as L
import Data.Tuple
import Data.Word
import Debug.Trace
import Numeric.Natural

import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Operand as Op
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as Type

import Utils (makeName, operandType, toLog)

data LLVMInstr = I (AST.Named AST.Instruction)
               | T (AST.Named AST.Terminator)
               deriving (Show)

unwrapT :: LLVMInstr -> AST.Named AST.Terminator
unwrapT (T t) = t
unwrapT instr = error $ "Not an LLVM terminator: " ++ show instr

unwrapI :: LLVMInstr -> AST.Named AST.Instruction
unwrapI (I i) = i
unwrapI term  = error $ "Not an LLVM instruction: " ++ show term

type NestedMap a = M.Map Name.Name (M.Map Name.Name a)

data OperandStack = OpS { currentBlock :: [AST.Operand]
                        , previousBlocks :: M.Map Name.Name [AST.Operand] }
                        deriving (Show)

-- a record for per-function state variables
data InstrST = InstrST { operandStack :: M.Map Name.Name OperandStack -- stack of operands associated to a block
                       , localIdentifier :: Natural
                       , blockIdentifier :: Natural
                       , funcIdentifier :: Name.Name
                       , blockScopeStack :: [Name.Name]
                       , localVariableTypes :: M.Map Name.Name Type.Type
                       , externalFuncs :: S.Set Name.Name
                       }
                       deriving (Show)

data FunctionType = FT { arguments :: [Type.Type], returnType :: Type.Type }
  deriving (Show)

-- a record for per-module constants
data ModEnv = ModEnv { startFunctionIndex :: Maybe Natural
                     , functionTypes :: M.Map Name.Name FunctionType
                     -- the types associated with the type aliases for call_indirect
                     , functionTypeIndices :: M.Map Natural FunctionType
                     , nativeWordWidth :: Word32
                     , memoryReference :: AST.Operand
                     , memoryMinSize :: Natural
                     , memoryMaxSize :: Maybe Natural
                     , tableReference :: AST.Operand
                     , tableElements :: M.Map Natural Name.Name
                     , tableRefType :: Type.Type
                     , exportedFunctions :: S.Set Name.Name
                     , functionIndexMap :: M.Map Natural Name.Name
                    --  , globalVariableTypes :: M.Map Natural Type.Type
                      }
                     deriving (Show)

newtype ModGen a = ModGen { _modGen :: ExceptT String (ReaderT ModEnv (Writer [String])) a }
  deriving (Functor, Applicative, Alternative, MonadPlus, Monad, MonadReader ModEnv, MonadWriter [String], MonadError String)

newtype InstrGen a = InstrGen { _funcGen :: StateT InstrST ModGen a }
  deriving (Functor, Applicative, Alternative, Monad, MonadReader ModEnv, MonadWriter [String], MonadError String, MonadState InstrST)

instance (Semigroup a) => Semigroup (ModGen a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (ModGen a) where
  mempty = pure mempty

instance (Semigroup a) => Semigroup (InstrGen a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (InstrGen a) where
  mempty = pure mempty

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a r = first (\e -> "Error: " ++ e ++ log) val
  where
    log = "\n\nLog:\n" ++ L.intercalate "\n" logLines ++ "\n<---Error"
    (val, logLines) = runWriter $ runReaderT (runExceptT $ _modGen a) r

evalInstrGen :: InstrGen a -> InstrST -> ModGen a
evalInstrGen a = evalStateT (_funcGen a)

popScope :: InstrGen ()
popScope = do
  scopeStack <- gets blockScopeStack
  (_, tail)  <- maybe (throwError "Empty scope stack") pure $ L.uncons scopeStack
  modify $ \st -> st { blockScopeStack = tail }

pushScope :: Name.Name -> InstrGen ()
pushScope s = modify $ \st -> st { blockScopeStack = s : blockScopeStack st }

readScope :: Int -> InstrGen Name.Name
readScope i = do
  scopeStack <- gets blockScopeStack
  maybe (throwError "Undefined scope") (pure . snd) $ L.find ((i==) . fst) $ zip [0..] scopeStack

-- pop operand off the `operandStack`
-- the rename action here should always be valid because of SSA and because we
-- do not reuse identifiers
popOperand :: InstrGen ([LLVMInstr], AST.Operand)
popOperand = do
  blockID      <- gets $ makeName "block" . blockIdentifier
  opStack      <- gets operandStack
  localStack   <- liftMaybe $ M.lookup blockID opStack
  (op, rest)   <- liftMaybe $ uncons blockID localStack
  modify $ \st -> st { operandStack = M.insert blockID rest opStack }
  (instrs, op) <- combine $ swap <$> op
  tell $ toLog "    pop operand--emit: " instrs
  tell $ toLog "    pop operand--op: " [op]
  pure (instrs, op)
    where
      liftMaybe = maybe (throwError "not enough operands") pure

      uncons _ (OpS [] map)
        | null map  = Nothing
        | otherwise = do unconned <- sequenceA $ L.uncons <$> map
                         let ops  = M.toList $ fst <$> unconned
                             rest = M.filter (not . null) $ snd <$> unconned
                         pure (ops, OpS [] rest)
      uncons blockID (OpS stack map) = do
        (op, rest) <- L.uncons stack
        pure ([(blockID, op)], OpS rest map)

      rename m (block, op) = (re op m, block)
      re (Op.LocalReference t name) = Op.LocalReference t . fromMaybe name . M.lookup name
      re op                         = const op

      combine []             = throwError "impossible branch"
      combine [(x, _)]       = pure ([], x)
      combine ops@((x, _):_) = do
        n <- gets localIdentifier
        let opType = operandType x
            ident  = makeName "wasmc.tmp" n
            instr  = ident AST.:= AST.Phi opType ops []
            op     = AST.LocalReference opType ident
        incrLocalIdentifier
        pure ([I instr], op)

pushOperand :: AST.Operand -> InstrGen ()
pushOperand op = do
  tell $ toLog "    push operand: " [op]
  blockID <- gets $ makeName "block" . blockIdentifier
  opStack <- gets operandStack
  let OpS stack map = fromMaybe (OpS [] M.empty) $ M.lookup blockID opStack
  modify $ \st -> st { operandStack = M.insert blockID (OpS (op:stack) map)   opStack }

pushOperands :: [AST.Operand] -> InstrGen ()
pushOperands = mapM_ pushOperand

peekOperand :: InstrGen ([LLVMInstr], AST.Operand)
peekOperand = do
  (phi, op) <- popOperand
  pushOperand op
  pure (phi, op)

deleteOperandStack :: Name.Name -> InstrGen ()
deleteOperandStack name = modify $ \st ->
  st { operandStack = M.delete name $ operandStack st }

branchOperandStack :: Name.Name -> Name.Name -> InstrGen [LLVMInstr]
branchOperandStack origin dest = do
  InstrST { operandStack, localIdentifier } <- get
  let originStack  = fromMaybe (OpS [] M.empty) $ M.lookup origin operandStack
      (OpS st m)   = fromMaybe (OpS [] M.empty) $ M.lookup dest operandStack
  (n, phis, ops) <- liftEither $ collapseStacks localIdentifier originStack
  let destStack    = OpS st $ M.insert origin ops m
  modify $ \st -> st { operandStack = M.insert dest destStack operandStack
                     , localIdentifier = n }
  pure phis

moveOperandStack :: Name.Name -> Name.Name -> InstrGen [LLVMInstr]
moveOperandStack origin dest = do
  phis <- branchOperandStack origin dest
  deleteOperandStack origin
  pure phis

collapseStacks :: Natural -> OperandStack -> Either String (Natural, [LLVMInstr], [AST.Operand])
collapseStacks n (OpS stack map) = do
  stacks         <- transpose $ toTupleList map
  (combined, n') <- runExcept $ runStateT (traverse combine stacks) n
  let (phis, operands) = unzip combined
  pure (n', I <$> phis, stack ++ operands)
    where
      combine []                 = throwError "empty sub-stack"
      combine ops@((fstOp, _):_) = do
        n <- get
        let ident  = makeName "wasmc.tmp" n
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
newNamedInstruction :: Type.Type -> AST.Instruction -> InstrGen (AST.Named AST.Instruction)
newNamedInstruction Type.VoidType instr = pure $ AST.Do instr
newNamedInstruction idType        instr = do
  name <- gets $ makeName "wasmc.tmp" . localIdentifier
  let identifier = AST.LocalReference idType name
  pushOperand identifier
  incrLocalIdentifier
  pure (name AST.:= instr)

returnOperandStackItems :: InstrGen ([LLVMInstr], AST.Named AST.Terminator)
returnOperandStackItems = do
  blockID        <- gets $ makeName "block" . blockIdentifier
  opStack        <- gets operandStack
  tmpID          <- gets localIdentifier
  let localStack = fromMaybe (OpS [] M.empty) $ M.lookup blockID opStack
  (n, phis, ops) <- liftEither $ collapseStacks tmpID localStack
  modify $ \st -> st { localIdentifier = n }
  (instrs, ret)  <- collapseOps ops
  pure (phis ++ fmap I instrs, AST.Do $ AST.Ret ret [])
    where
      collapseOps []  = pure ([], Nothing)
      collapseOps [x] = pure ([], Just x)
      collapseOps ops = do
        index                        <- gets funcIdentifier
        FT { arguments, returnType } <- asks (flip (M.!) index . functionTypes)
        let ptrIdent  = makeName "wasmc.local" $ fromIntegral $ length arguments
            ptrType   = Type.PointerType returnType $ Addr.AddrSpace 0
            ptr       = AST.LocalReference ptrType ptrIdent
            genInstr (i, op, t) = do
              let idx     = AST.ConstantOperand $ Constant.Int 32 i
              getPtrInstr <- newNamedInstruction t $ AST.GetElementPtr False ptr [idx] []
              (_, addr)   <- popOperand  -- no phi
              let storeInstr = AST.Do $ AST.Store False addr op Nothing 0 []
              pure [getPtrInstr, storeInstr]
        instrs <- foldMap genInstr $ zip3 [0..] ops $ AST.elementTypes returnType
        tell $ toLog "    collapse-ops: " instrs
        pure (instrs, Nothing)
