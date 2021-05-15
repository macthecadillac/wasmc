module Codegen where

import Data.Char (isAlpha)
import qualified Data.List as L
import Data.Maybe
import Data.Word

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

data ArgType = R Register | I Word32 | S String
instance Show ArgType where
  show (R r) = show r
  show (I n) = show n
  show (S s) = s

make :: String -> [ArgType] -> String
make name l = name ++ " " ++ L.intercalate "," (show <$> l)

makeP :: String -> [ArgType] -> String
makeP name [] = name
makeP name l  = name ++ " " ++ L.intercalate "," (show <$> reverse rest) ++ "(" ++ show last_ ++ ")"
  where
    (last_, rest) = fromJust $ L.uncons $ reverse l

generateInstruction :: MIPSInstruction -> String
generateInstruction Empty = ""
generateInstruction (Label   labelName)             = labelName ++ ":"
generateInstruction (Comment comment)               = "# " ++ comment
generateInstruction (Inst    SYSCALL)               = "syscall"
-- generateInstruction (Inst    LIT_ASM)               =
generateInstruction (Inst    (OP_ADD     rx ry rz)) = make "add"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MOVE    rx ry))    = make "move"    [R rx, R ry]
generateInstruction (Inst    (OP_LI      rx iy))    = make "li"      [R rx, I iy]
generateInstruction (Inst    (OP_LA      rx sy))    = make "la"      [R rx, S sy]
generateInstruction (Inst    (OP_MUL     rx ry rz)) = make "mul"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_LW      rx iy rz)) = makeP "lw"     [R rx, I iy, R rz]
generateInstruction (Inst    (OP_SW      rx iy rz)) = makeP "sw"     [R rx, I iy, R rz]
generateInstruction (Inst    (OP_LB      rx sy))    = make "lb"      [R rx, S sy]
generateInstruction (Inst    (OP_SB      rx sy))    = make "sb"      [R rx, S sy]
generateInstruction (Inst    (OP_XOR     rx ry rz)) = make "xor"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_DIV     rx ry))    = make "div"     [R rx, R ry]
generateInstruction (Inst    (OP_SUB     rx ry rz)) = make "sub"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_AND     rx ry rz)) = make "and"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_OR      rx ry rz)) = make "or"      [R rx, R ry, R rz]
generateInstruction (Inst    (OP_BNE     rx ry nz)) = make "bne"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_BEQ     rx ry nz)) = make "beq"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_BGT     rx ry nz)) = make "bgt"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_BGE     rx ry nz)) = make "bge"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_BLT     rx ry nz)) = make "blt"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_BLE     rx ry nz)) = make "ble"     [R rx, R ry, I nz]
generateInstruction (Inst    (OP_SEQ     rx ry rz)) = make "seq"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SGE     rx ry rz)) = make "sge"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SGT     rx ry rz)) = make "sgt"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SLE     rx ry rz)) = make "sle"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SLT     rx ry rz)) = make "slt"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SNE     rx ry rz)) = make "sne"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SGEU    rx ry rz)) = make "sgeu"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SGTU    rx ry rz)) = make "sgtu"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SLEU    rx ry rz)) = make "sleu"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SLTU    rx ry rz)) = make "sltu"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_J       nx))       = make "j"       [I nx]
generateInstruction (Inst    (OP_JR      rx))       = make "jr"      [R rx]
generateInstruction (Inst    (OP_JAL     sx))       = make "jal"     [S sx]
generateInstruction (Inst    (OP_JALR    rx))       = make "jalr"    [R rx]
generateInstruction (Inst    (OP_SLLV    rx ry rz)) = make "sllv"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SRLV    rx ry rz)) = make "srlv"    [R rx, R ry, R rz]
generateInstruction (Inst    (OP_REM     rx ry rz)) = make "rem"     [R rx, R ry, R rz]
generateInstruction (Inst    (OP_NOT     rx ry))    = make "not"     [R rx, R ry]
generateInstruction (Inst    (OP_ADDS    rx ry rz)) = make "add.s"   [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MULS    rx ry rz)) = make "mul.s"   [R rx, R ry, R rz]
generateInstruction (Inst    (OP_DIVS    rx ry rz)) = make "div.s"   [R rx, R ry, R rz]
generateInstruction (Inst    (OP_SUBS    rx ry rz)) = make "sub.s"   [R rx, R ry, R rz]
generateInstruction (Inst    (OP_MOVS    rx ry))    = make "mov.s"   [R rx, R ry]
generateInstruction (Inst    (OP_MTC1    rx ry))    = make "mtc1"    [R rx, R ry]
generateInstruction (Inst    (OP_MFC1    rx ry))    = make "mfc1"    [R rx, R ry]
generateInstruction (Inst    (OP_CVT_W_S rx ry))    = make "cwt.w.s" [R rx, R ry]
generateInstruction (Inst    (OP_CVT_S_W rx ry))    = make "cwt.s.w" [R rx, R ry]
generateInstruction (Inst    (OP_CEQS    rx ry))    = make "c.eq.s"  [R rx, R ry]
generateInstruction (Inst    (OP_CLES    rx ry))    = make "c.le.s"  [R rx, R ry]
generateInstruction (Inst    (OP_CLTS    rx ry))    = make "c.lt.s"  [R rx, R ry]
generateInstruction (Inst    (OP_BC1F    sx))       = make "bc1f"    [S sx]
generateInstruction (Inst    (OP_BC1T    sx))       = make "bc1t"    [S sx]
generateInstruction (Inst    (OP_LS      rx iy))    = make "l.s"     [R rx, I iy]
generateInstruction (Inst    (OP_SS      rx iy))    = make "s.s"     [R rx, I iy]
generateInstruction (Inst    (OP_LIS     rx iy))    = make "li.s"    [R rx, I iy]
generateInstruction (Inst    (OP_MTC0    rx ry))    = make "mtc0"    [R rx, R ry]
generateInstruction (Inst    (OP_ADDIU   rx ry nz)) = make "addiu"   [R rx, R ry, I nz]