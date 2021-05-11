module Instructions where

import Data.Bits
import Numeric.Natural

data MIPSFile = MIPSFile String [MIPSSection] [[MIPSInstruction]] -- Each list contains one function def.
  deriving Show

data MIPSSection = MIPSSection String [(String, String, String)]
  deriving Show

data MIPSInstruction = Inst MIPSOp
                     | Label String
                     | Comment String
                     | Empty
                     deriving (Eq, Show)

-- http://www.cs.uwm.edu/classes/cs315/Bacon/Lecture/HTML/ch05s03.html
-- TODO: Bounds check -- safety related
data Register = Zero
              | At
              | Res Natural
              | Arg Natural
              | Tmp Natural
              | Save Natural
              | GP
              | SP
              | FP
              | Ret
              deriving (Eq)

instance Show Register where
  show Zero     = "$zero"
  show At       = "$at"
  show (Res n)  = "$v" ++ show n
  show (Arg n)  = "$a" ++ show n
  show (Save n) = "$s" ++ show n
  show (Tmp n)  = "$t" ++ show n
  show GP       = "$gp"
  show SP       = "$sp"
  show FP       = "$fp"
  show Ret      = "$ra"

data MIPSOp = OP_ADD Register Register Register
            | OP_MOVE Register Register
            | OP_LI Register Natural
            | OP_LA Register String
            | OP_MUL Register Register Register
            | OP_LW Register Natural Register
            | OP_SW Register Natural Register
            | OP_LB Register String
            | OP_SB Register String
            | OP_XOR Register Register Register
            | OP_DIV Register Register
            | OP_SUB Register Register Register
            | OP_AND Register Register Register
            | OP_OR Register Register Register
            | OP_BNE Register Register Natural
            | OP_BEQ Register Register Natural
            | OP_BGT Register Register Natural
            | OP_BGE Register Register Natural
            | OP_BLT Register Register Natural
            | OP_BLE Register Register Natural
            | OP_J Natural
            | OP_JR Register
            | OP_JAL Natural
            | OP_JALR Register
            | OP_SLL Register Register Natural
            | OP_SRL Register Register Natural
            | OP_REM Register Register Register
            | OP_NOT Register Register
            | SYSCALL
            | LIT_ASM -- Used for inlining assembly.
            -- Float operations
            | OP_ADDS Register Register Register
            | OP_MULS Register Register Register
            | OP_DIVS Register Register Register
            | OP_SUBS Register Register Register
            | OP_MOVS Register Register
            | OP_MTC1 Register Register-- Move to float processor.
            | OP_MFC1 Register Register-- Move from float processor.
            | OP_CVT_W_S Register Register-- Convert float to int (word, single)
            | OP_CVT_S_W Register Register-- Convert int to float (single, word)
            | OP_CEQS Register Register -- Set code to 1 if equal.
            | OP_CLES Register Register -- Set code to 1 if less than or equal to
            | OP_CLTS Register Register -- Set code to 1 if less than
            | OP_BC1F String-- Branch if code == 0
            | OP_BC1T String-- Branch if code == 1
            | OP_LS Register Natural -- Load single
            | OP_SS Register Natural -- Store single
            | OP_LIS Register Natural -- Load immediate single.
            | OP_MTC0 Register Register
            | OP_ADDIU Register Register Natural
            | OP_SUBIU Register Register Natural
            deriving (Show, Eq)

-- opList = [OP_ADD, OP_MOVE, OP_LI, OP_LA, OP_MUL,
--       OP_LW, OP_SW, OP_LB, OP_SB, OP_XOR, OP_DIV,
--       OP_SUB, OP_AND, OP_OR, OP_BNE, OP_BEQ,
--       OP_BGT, OP_BGE, OP_BLT, OP_BLE, OP_J,
--       OP_JR, OP_JAL, OP_JALR, OP_SLL, OP_SRL,
--       OP_REM, OP_NOT, SYSCALL, OP_ADDS, OP_MULS,
--       OP_SUBS, OP_MOVS, OP_MTC1, OP_MFC1, OP_CVT_W_S,
--       OP_CVT_S_W, OP_CEQS, OP_CLES, OP_CLTS, OP_BC1F,
--       OP_BC1T, OP_LS, OP_SS, OP_LIS, OP_MTC0]

-- isLabel (Label _) = True
-- isLabel _ = False

-- is :: MIPSOp -> MIPSInstruction -> Bool
-- is opName (Inst op _ _ _) = opName == op
-- is _ _ = False

-- isCall::MIPSInstruction -> Bool
-- isCall (Inst OP_JAL _ _ _) = True
-- isCall (Inst OP_JALR _ _ _) = True
-- isCall (Inst SYSCALL _ _ _) = True
-- isCall _ = False

-- hasOperand :: (String -> Bool) -> MIPSInstruction -> Bool
-- hasOperand f (Inst _ a b c) = f a || f b || f c
-- hasOperand _ _ = False

-- getOperands :: MIPSInstruction -> [String]
-- getOperands (Inst _ a b c) = [a,b,c]
-- getOperands _ = []

-- setOperands :: MIPSInstruction -> [String] -> MIPSInstruction
-- setOperands (Inst op _ _ _) [a,b,c] = Inst op a b c
-- setOperands i _ = i

-- replaceOperand :: String -> String -> MIPSInstruction -> MIPSInstruction
-- replaceOperand search rep instr = setOperands instr $ map (\a -> if a == search then rep else a) $ getOperands instr

-- branchTarget :: MIPSInstruction -> Maybe String
-- branchTarget (Inst OP_J target _ _) = Just target
-- branchTarget (Inst OP_JAL target _ _) = Just target
-- branchTarget (Inst OP_BNE _ _ target) = Just target
-- branchTarget (Inst OP_BEQ _ _ target) = Just target
-- branchTarget (Inst OP_BGT _ _ target) = Just target
-- branchTarget (Inst OP_BGE _ _ target) = Just target
-- branchTarget (Inst OP_BLT _ _ target) = Just target
-- branchTarget (Inst OP_BLE _ _ target) = Just target
-- branchTarget (Inst OP_BC1F target _ _) = Just target
-- branchTarget (Inst OP_BC1T target _ _) = Just target
-- branchTarget _ = Nothing

-- isArith :: MIPSInstruction -> Bool
-- isArith (Inst OP_ADD _ _ _) = True
-- isArith (Inst OP_MUL _ _ _) = True
-- isArith (Inst OP_XOR _ _ _) = True
-- isArith (Inst OP_DIV _ _ _) = True
-- isArith (Inst OP_SUB _ _ _) = True
-- isArith (Inst OP_AND _ _ _) = True
-- isArith (Inst OP_OR _ _ _) = True
-- isArith (Inst OP_REM _ _ _) = True
-- isArith (Inst OP_SLL _ _ _) = True
-- isArith (Inst OP_SRL _ _ _) = True
-- isArith _ = False

-- isArithFloat :: MIPSInstruction -> Bool
-- isArithFloat (Inst OP_SUBS _ _ _) = True
-- isArithFloat (Inst OP_DIVS _ _ _) = True
-- isArithFloat (Inst OP_ADDS _ _ _) = True
-- isArithFloat (Inst OP_MULS _ _ _) = True
-- isArithFloat _ = False

-- commutes OP_MUL = True
-- commutes OP_ADD = True
-- commutes OP_AND = True
-- commutes OP_OR = True
-- commutes OP_XOR = True
-- commutes _ = False

-- compute :: MIPSOp -> Int -> Int -> Int
-- compute OP_ADD = (+)
-- compute OP_MUL = (*)
-- compute OP_XOR = xor
-- compute OP_DIV = div
-- compute OP_SUB = (-)
-- compute OP_AND = (.&.)
-- compute OP_OR = (.|.)
-- compute OP_REM = mod
-- compute OP_SRL = shiftR
-- compute OP_SLL = shiftL

-- computeFloat :: MIPSOp -> Float -> Float -> Float
-- computeFloat OP_SUBS = (-)
-- computeFloat OP_DIVS = (/)
-- computeFloat OP_ADDS = (+)
-- computeFloat OP_MULS = (*)

-- isBranch (Inst OP_BNE _ _ _) = True
-- isBranch (Inst OP_BEQ _ _ _) = True
-- isBranch (Inst OP_BGT _ _ _) = True
-- isBranch (Inst OP_BGE _ _ _) = True
-- isBranch (Inst OP_BLT _ _ _) = True
-- isBranch (Inst OP_BLE _ _ _) = True
-- isBranch _ = False

-- isBranchFloat (Inst OP_CEQS _ _ _) = True
-- isBranchFloat (Inst OP_CLES _ _ _) = True
-- isBranchFloat (Inst OP_CLTS _ _ _) = True
-- isBranchFloat _ = False

-- isJump (Inst OP_BNE _ _ _) = True
-- isJump (Inst OP_BEQ _ _ _) = True
-- isJump (Inst OP_BGT _ _ _) = True
-- isJump (Inst OP_BGE _ _ _) = True
-- isJump (Inst OP_BLT _ _ _) = True
-- isJump (Inst OP_BLE _ _ _) = True
-- isJump (Inst OP_J _ _ _) = True
-- isJump (Inst OP_JR _ _ _) = True
-- isJump (Inst OP_JAL _ _ _) = True
-- isJump (Inst OP_JALR _ _ _) = True
-- isJump (Inst OP_BC1F _ _ _) = True
-- isJump (Inst OP_BC1T _ _ _) = True
-- isJump _ = False

-- checkBranch OP_BNE = (/=)
-- checkBranch OP_BEQ = (==)
-- checkBranch OP_BGT = (>)
-- checkBranch OP_BGE = (>=)
-- checkBranch OP_BLT = (<)
-- checkBranch OP_BLE = (<=)

-- checkBranchFloat OP_CEQS = (==)
-- checkBranchFloat OP_CLES = (<=)
-- checkBranchFloat OP_CLTS = (<)

-- isFloatInstr :: MIPSInstruction -> Bool
-- isFloatInstr (Inst OP_ADDS _ _ _) = True
-- isFloatInstr (Inst OP_MULS _ _ _) = True
-- isFloatInstr (Inst OP_DIVS _ _ _) = True
-- isFloatInstr (Inst OP_SUBS _ _ _) = True
-- isFloatInstr (Inst OP_MOVS _ _ _) = True
-- isFloatInstr (Inst OP_MTC1 _ _ _) = True
-- -- This one doesn't count because it's result isn't a float.
-- -- isFloatInstr (Inst OP_MFC1 _ _ _) = True
-- isFloatInstr (Inst OP_CVT_W_S _ _ _) = True
-- isFloatInstr (Inst OP_CVT_S_W _ _ _) = True
-- isFloatInstr (Inst OP_CEQS _ _ _) = True
-- isFloatInstr (Inst OP_CLES _ _ _) = True
-- isFloatInstr (Inst OP_CLTS _ _ _) = True
-- isFloatInstr (Inst OP_BC1F _ _ _) = True
-- isFloatInstr (Inst OP_BC1T _ _ _) = True
-- isFloatInstr (Inst OP_LS _ _ _) = True
-- isFloatInstr (Inst OP_SS _ _ _) = True
-- isFloatInstr (Inst OP_LIS  _ _ _) = True
-- isFloatInstr _ = False

-- instResult :: MIPSInstruction -> Maybe String
-- instResult (Inst OP_ADD a _ _) = Just a
-- instResult (Inst OP_MOVE a _ _) = Just a
-- instResult (Inst OP_MUL a _ _) = Just a
-- instResult (Inst OP_LW a _ _) = Just a
-- instResult (Inst OP_LB a _ _) = Just a
-- instResult (Inst OP_XOR a _ _) = Just a
-- instResult (Inst OP_DIV a _ _) = Just a
-- instResult (Inst OP_SUB a _ _) = Just a
-- instResult (Inst OP_AND a _ _) = Just a
-- instResult (Inst OP_OR a _ _) = Just a
-- instResult (Inst OP_SLL a _ _) = Just a
-- instResult (Inst OP_SRL a _ _) = Just a
-- instResult (Inst OP_REM a _ _) = Just a
-- instResult (Inst OP_NOT a _ _) = Just a
-- instResult (Inst OP_LI a _ _) = Just a
-- instResult (Inst OP_LA a _ _) = Just a
-- instResult (Inst OP_ADDS a _ _) = Just a
-- instResult (Inst OP_MULS a _ _) = Just a
-- instResult (Inst OP_DIVS a _ _) = Just a
-- instResult (Inst OP_SUBS a _ _) = Just a
-- instResult (Inst OP_MOVS a _ _) = Just a
-- instResult (Inst OP_MTC1 _ b _) = Just b
-- instResult (Inst OP_MFC1 a _ _) = Just a
-- instResult (Inst OP_CVT_W_S a _ _) = Just a
-- instResult (Inst OP_CVT_S_W a _ _) = Just a
-- instResult (Inst OP_LS a _ _) = Just a
-- instResult (Inst OP_LIS a _ _) = Just a
-- instResult _ = Nothing

-- instUses :: MIPSInstruction -> [String]
-- instUses (Inst OP_MOVE _ b _) = [b]
-- instUses (Inst OP_ADD _ b c) = [b,c]
-- instUses (Inst OP_MUL _ b c) = [b,c]
-- instUses (Inst OP_LW a _ c) = [c]
-- instUses (Inst OP_SW a _ c) = [a,c]
-- instUses (Inst OP_LB a _ c) = [c]
-- instUses (Inst OP_SB a _ c) = [a,c]
-- instUses (Inst OP_XOR _ b c) = [b,c]
-- instUses (Inst OP_DIV _ b c) = [b,c]
-- instUses (Inst OP_SUB _ b c) = [b,c]
-- instUses (Inst OP_AND _ b c) = [b,c]
-- instUses (Inst OP_OR _ b c) = [b,c]
-- instUses (Inst OP_BNE a b _) = [a,b]
-- instUses (Inst OP_BEQ a b _) = [a,b]
-- instUses (Inst OP_BGT a b _) = [a,b]
-- instUses (Inst OP_BGE a b _) = [a,b]
-- instUses (Inst OP_BLT a b _) = [a,b]
-- instUses (Inst OP_BLE a b _) = [a,b]
-- instUses (Inst OP_JR a _ _) = [a]
-- instUses (Inst OP_JALR a _ _) = [a]
-- instUses (Inst OP_SLL _ b c) = [b,c]
-- instUses (Inst OP_SRL _ b c) = [b,c]
-- instUses (Inst OP_REM _ b c) = [b,c]
-- instUses (Inst OP_NOT _ b _) = [b]
-- instUses (Inst OP_ADDS _ b c) = [b,c]
-- instUses (Inst OP_MULS _ b c) = [b,c]
-- instUses (Inst OP_DIVS _ b c) = [b,c]
-- instUses (Inst OP_SUBS _ b c) = [b,c]
-- instUses (Inst OP_MOVS _ b _) = [b]
-- instUses (Inst OP_CVT_W_S _ b _) = [b]
-- instUses (Inst OP_CVT_S_W _ b _) = [b]
-- instUses (Inst OP_CEQS a b _) = [a,b]
-- instUses (Inst OP_CLES a b _) = [a,b]
-- instUses (Inst OP_CLTS a b _) = [a,b]
-- instUses (Inst OP_LS _ _ c) = [c]
-- instUses (Inst OP_SS a _ c) = [a,c]

-- -- Unlisted instructions are assumed to use all of their registers.
-- instUses (Inst _ a b c) = [a, b, c]
-- instUses _ = []
