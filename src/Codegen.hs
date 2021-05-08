module Codegen where

import Data.Char (isAlpha)
import qualified Data.List as L

import Instructions

printCode :: MIPSFile -> IO ()
printCode = putStrLn . generateFile

generateFile :: MIPSFile -> String
generateFile (MIPSFile _ sections instructions) =
  intercalate "\n\n" (filter (not . null) (map generateSection sections)) ++ "\n" ++
  intercalate "\n\n" (map generate instructions)

generateSection :: MIPSSection -> String
generateSection (MIPSSection "text" _) = ".text"
generateSection (MIPSSection _ []) = ""
generateSection (MIPSSection name d) =
  "." ++ name ++ "\n" ++
  intercalate "\n" (map generateData d)

generateData :: (String, String, String) -> String
generateData (name, dataType, dataVal) =
  case dataType of
    "asciiz" -> name ++ ": " ++ "." ++ dataType ++ " " ++ show dataVal
    _ -> name ++ ": " ++ "." ++ dataType ++ " " ++ dataVal

generate :: [MIPSInstruction] -> String
generate [] = ""
generate (inst:instructions) =
  generateInstruction inst ++ "\n" ++
  intercalate "\n" (map (("  " ++) . generateInstruction) instructions)

regName :: String -> String
regName "" = ""
regName reg
  | not $ isRegister reg = reg
  | otherwise = "$" ++ reg

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
generateInstruction (Label labelName) = labelName ++ ":"
generateInstruction (Comment comment) = "# " ++ comment
generateInstruction (Inst (OP_ADD r1 r2 r3)) = show3 "add" (R r1) (R r2) (R r3)
generateInstruction (Inst (OP_MOVE r1 r2)) = show2 "move" (R r1) (R r2)
generateInstruction (Inst (OP_LI r n)) = show2 "li" (R r) (N n)
generateInstruction (Inst (OP_LA r s)) = show2 "la" (R r) (S s)
genreateInstruction (Inst (OP_MUL r1 r2 r3)) = show3 "mul" (R r1) (R r2) (R r3)
generateInstruction (Inst (OP_LW r1 n r2)) = show3 "lw" (R r1) (N n) (R r2)

generateInstruction (Inst SYSCALL) = "syscall"