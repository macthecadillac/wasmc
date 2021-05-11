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

make :: String -> [ArgType] -> String
make name l = name ++ " " ++ L.intercalate ", " (show <$> l)

generateInstruction :: MIPSInstruction -> String
generateInstruction Empty = ""
generateInstruction (Label   labelName)             = labelName ++ ":"
generateInstruction (Comment comment)               = "# " ++ comment
generateInstruction (Inst    SYSCALL)               = "syscall"
-- generateInstruction (Inst    LIT_ASM)               =
generateInstruction (Inst    (OP_ADD     rx ry rz)) = make "add"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MOVE    rx ry))    = make "move"   [R rx, R ry]
generateInstruction (Inst    (OP_LI      rx ny))    = make "li"     [R rx, N ny]
generateInstruction (Inst    (OP_LA      rx sy))    = make "la"     [R rx, S sy]
generateInstruction (Inst    (OP_MUL     rx ry rz)) = make "mul"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_LW      rx ny rz)) = make "lw"     [R rx, N ny, R rz]
generateInstruction (Inst    (OP_SW      rx ny rz)) = make "sw"     [R rx, N ny, R rz]
generateInstruction (Inst    (OP_LB      rx sy))    = make "lb"     [R rx, S sy]
generateInstruction (Inst    (OP_SB      rx sy))    = make "sb"     [R rx, S sy]
generateInstruction (Inst    (OP_XOR     rx ry rz)) = make "xor"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_DIV     rx ry))    = make "div"    [R rx, R ry]
generateInstruction (Inst    (OP_SUB     rx ry rz)) = make "sub"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_AND     rx ry rz)) = make "and"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_OR      rx ry rz)) = make "or"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_BNE     rx ry nz)) = make "bne"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_BEQ     rx ry nz)) = make "beq"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_BGT     rx ry nz)) = make "bgt"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_BGE     rx ry nz)) = make "bge"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_BLT     rx ry nz)) = make "blt"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_BLE     rx ry nz)) = make "ble"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_J       nx))       = make "j"      [N nx]
generateInstruction (Inst    (OP_JR      rx))       = make "jr"     [R rx]
generateInstruction (Inst    (OP_JAL     nx))       = make "jal"    [N nx]
generateInstruction (Inst    (OP_JALR    rx))       = make "jalr"   [R rx]
generateInstruction (Inst    (OP_SLL     rx ry nz)) = make "sll"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_SRL     rx ry nz)) = make "srl"    [R rx, R ry, N nz]
generateInstruction (Inst    (OP_REM     rx ry rz)) = make "rem"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_NOT     rx ry))    = make "not"    [R rx, R ry]
generateInstruction (Inst    (OP_ADDS    rx ry rz)) = make "add.s"  [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MULS    rx ry rz)) = make "mul.s"  [R rx, R ry, R rz]
generateInstruction (Inst    (OP_DIVS    rx ry rz)) = make "div.s"  [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SUBS    rx ry rz)) = make "sub.s"  [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MOVS    rx ry))    = make "mov.s"  [R rx, R ry]
generateInstruction (Inst    (OP_MTC1    rx ry))    = make "mtc1"   [R rx, R ry]
generateInstruction (Inst    (OP_MFC1    rx ry))    = make "mfc1"   [R rx, R ry]
generateInstruction (Inst    (OP_CVT_W_S      rx ny))    = make "cwt.w.s"     [R rx, N ny]
generateInstruction (Inst    (OP_CVT_S_W      rx ny))    = make "cwt.s.w"     [R rx, N ny]
generateInstruction (Inst    (OP_CEQS    rx ry))    = make "c.eq.s" [R rx, R ry]
generateInstruction (Inst    (OP_CLES    rx ry))    = make "c.le.s" [R rx, R ry]
generateInstruction (Inst    (OP_CLTS    rx ry))    = make "c.lt.s" [R rx, R ry]
generateInstruction (Inst    (OP_BC1F    sx)) = make "bc1f"  [S sx]
generateInstruction (Inst    (OP_BC1T    sx)) = make "bc1t"  [S sx]
generateInstruction (Inst    (OP_LS    rx ny))    = make "l.s"  [R rx, N ny]
generateInstruction (Inst    (OP_SS    rx ny))    = make "s.s"   [R rx, N ny]
generateInstruction (Inst    (OP_LIS    rx ny))    = make "li.s"   [R rx, N ny]
generateInstruction (Inst    (OP_MTC0    rx ry))    = make "mtc0"   [R rx, R ry]
generateInstruction (Inst    (OP_ADDIU   rx ry nz)) = make "addiu"  [R rx, R ry, N nz]
generateInstruction (Inst    (OP_SUBIU   rx ry nz)) = make "subiu"  [R rx, R ry, N nz]
