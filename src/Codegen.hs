module Codegen where

import Data.Char (isAlpha)
import qualified Data.List as L
import Numeric.Natural

import Instructions

printCode :: MIPSFile -> IO ()
printCode = putStrLn . generateFile

generateFile :: MIPSFile -> String
generateFile (MIPSFile _ sections instructions) =
  L.intercalate "\n\n" (filter (not . null) (map generateSection sections)) ++ "\n" ++
  L.intercalate "\n\n" (map generate instructions)

generateSection :: MIPSSection -> String
generateSection (MIPSSection "text" _) = ".text"
generateSection (MIPSSection _ []) = ""
generateSection (MIPSSection name d) =
  "." ++ name ++ "\n" ++
  L.intercalate "\n" (map generateData d)

generateData :: (String, String, String) -> String
generateData (name, dataType, dataVal) =
  case dataType of
    "asciiz" -> name ++ ": " ++ "." ++ dataType ++ " " ++ show dataVal
    _ -> name ++ ": " ++ "." ++ dataType ++ " " ++ dataVal

generate :: [MIPSInstruction] -> String
generate [] = ""
generate (inst:instructions) =
  generateInstruction inst ++ "\n" ++
  L.intercalate "\n" (map (("  " ++) . generateInstruction) instructions)

data ArgType = R Register | N Natural | S String
instance Show ArgType where
  show (R r) = show r
  show (N n) = show n
  show (S s) = s

show1 :: String -> ArgType -> String
show1 name r1 = name ++ " " ++ show r1

show2 :: String ->  ArgType -> ArgType -> String
show2 name r1 r2 = name ++ " " ++ L.intercalate ", " (show <$> [r1, r2])

show3 :: String -> ArgType -> ArgType -> ArgType -> String
show3 name r1 r2 r3 = name ++ " " ++ L.intercalate ", " (show <$> [r1, r2, r3])

generateInstruction :: MIPSInstruction -> String
generateInstruction Empty = ""
generateInstruction (Label   labelName)             = labelName ++ ":"
generateInstruction (Comment comment)               = "# " ++ comment
generateInstruction (Inst    SYSCALL)               = "syscall"
generateInstruction (Inst    LIT_ASM)               = "rd"
generateInstruction (Inst    (OP_ADD     rx ry rz)) = show3 "add"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_MOVE    rx ry))    = show2 "move"   (R rx) (R ry)
generateInstruction (Inst    (OP_LI      rx ny))    = show2 "li"     (R rx) (N ny)
generateInstruction (Inst    (OP_LA      rx sy))    = show2 "la"     (R rx) (S sy)
generateInstruction (Inst    (OP_MUL     rx ry rz)) = show3 "mul"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_LW      rx ny rz)) = show3 "lw"     (R rx) (N ny) (R rz)
generateInstruction (Inst    (OP_SW      rx ny rz)) = show3 "sw"     (R rx) (N ny) (R rz)
generateInstruction (Inst    (OP_LB      rx sy))    = show2 "lb"     (R rx) (S sy)
generateInstruction (Inst    (OP_SB      rx sy))    = show2 "sb"     (R rx) (S sy)
generateInstruction (Inst    (OP_XOR     rx ry rz)) = show3 "xor"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_DIV     rx ry))    = show2 "div"    (R rx) (R ry)
generateInstruction (Inst    (OP_SUB     rx ry rz)) = show3 "sub"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_AND     rx ry rz)) = show3 "and"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_OR      rx ry rz)) = show3 "or"     (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_BNE     rx ry nz)) = show3 "bne"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_BEQ     rx ry nz)) = show3 "beq"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_BGT     rx ry nz)) = show3 "bgt"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_BGE     rx ry nz)) = show3 "bge"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_BLT     rx ry nz)) = show3 "blt"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_BLE     rx ry nz)) = show3 "ble"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_J       nx))       = show1 "j"      (N nx)
generateInstruction (Inst    (OP_JR      rx))       = show1 "jr"     (R rx)
generateInstruction (Inst    (OP_JAL     nx))       = show1 "jal"    (N nx)
generateInstruction (Inst    (OP_JALR    rx))       = show1 "jalr"   (R rx)
generateInstruction (Inst    (OP_SLL     rx ry nz)) = show3 "sll"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_SRL     rx ry nz)) = show3 "srl"    (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_REM     rx ry rz)) = show3 "rem"    (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_NOT     rx ry))    = show2 "not"    (R rx) (R ry)
generateInstruction (Inst    (OP_ADDS    rx ry rz)) = show3 "add.s"  (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_MULS    rx ry rz)) = show3 "mul.s"  (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_DIVS    rx ry rz)) = show3 "div.s"  (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_SUBS    rx ry rz)) = show3 "sub.s"  (R rx) (R ry) (R rz)
generateInstruction (Inst    (OP_MOVS    rx ry))    = show2 "mov.s"  (R rx) (R ry)
generateInstruction (Inst    (OP_MTC1    rx ry))    = show2 "mtc1"   (R rx) (R ry)
generateInstruction (Inst    (OP_MFC1    rx ry))    = show2 "mfc1"   (R rx) (R ry)
-- generateInstruction (Inst    (OP_CVT_W_S))          =
-- generateInstruction (Inst    (OP_CVT_S_W))          =
generateInstruction (Inst    (OP_CEQS    rx ry))    = show2 "c.eq.s" (R rx) (R ry)
generateInstruction (Inst    (OP_CLES    rx ry))    = show2 "c.le.s" (R rx) (R ry)
generateInstruction (Inst    (OP_CLTS    rx ry))    = show2 "c.lt.s" (R rx) (R ry)
-- generateInstruction (Inst    (OP_BC1F))             =
-- generateInstruction (Inst    (OP_BC1T))             =
-- generateInstruction (Inst    (OP_LS))               =
-- generateInstruction (Inst    (OP_SS))               =
-- generateInstruction (Inst    (OP_LIS))              =
generateInstruction (Inst    (OP_MTC0    rx ry))    = show2 "mtc0"   (R rx) (R ry)
generateInstruction (Inst    (OP_ADDIU   rx ry nz)) = show3 "addiu"  (R rx) (R ry) (N nz)
generateInstruction (Inst    (OP_SUBIU   rx ry nz)) = show3 "subiu"  (R rx) (R ry) (N nz)
