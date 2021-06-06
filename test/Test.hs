{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Language.Wasm.Structure as S
import Numeric.Natural

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as B
import Data.Either
import Data.Functor
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO
import Debug.Trace
import System.IO as IO
import System.Directory
import System.Exit (ExitCode(..))
import System.Process
import System.Random

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.AST (Module(..))
import LLVM.Pretty (ppllvm)

import Compiler
import Common
import Spec

main :: IO ()
main = defaultMain . testGroup "tests" . evalState (sequence tests) =<< newStdGen
    where
      tests = [binops, unops, mathFuncs, controls, loadStores, tables]

binops :: State StdGen TestTree
binops = testGroup "binop" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ binop1
            , binop2
            , binop3
            , binop4
            , binop5
            , binop6
            , binop7
            , binop8
            , binop9
            , binop10
            , binop11
            , binop12
            ]

unops :: State StdGen TestTree
unops = testGroup "unops" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ unop1, unop2 ]

mathFuncs :: State StdGen TestTree
mathFuncs = testGroup "math" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ math1
            , math2
            , math3
            , math4
            , math5
            , math6
            , math7
            , math8
            , math9
            , math10
            ]

controls :: State StdGen TestTree
controls = testGroup "controls" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ control1, control2 ]

loadStores :: State StdGen TestTree
loadStores = testGroup "load-store" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ loadStore1 ]

tables :: State StdGen TestTree
tables = testGroup "table access" <$> testCases
  where
    testCases = join <$> traverse buildTestCase tests
    tests = [ table1 ]

rustStub :: Interface -> [([Val], Val)] -> [T.Text]
rustStub (Interface n a r) = fmap $ T.pack . intercalate "\n" . uncurry rustSrc
  where
    argList args = intercalate ", " $ show <$> args
    argTypes     = intercalate ", " ["arg" ++ show i ++ ": " ++ show t | (i, t) <- zip [0..] a]
    retType      = maybe "" ((" -> " ++) . show) r
    decl         = "    fn " ++ n ++ "(" ++ argTypes ++ ")" ++ retType ++ ";"
    verify       = case r of
                     Just I32 -> intercalate "\n" . intVerify
                     Just I64 -> intercalate "\n" . intVerify
                     Just F32 -> intercalate "\n" . floatVerify
                     Just F64 -> intercalate "\n" . floatVerify
                     Nothing  -> const ""
    print_stdout res = [ "        print!(\"passed\");"
                       , "    } else {"
                       , "        print!(\"expected {}, got {}\", " ++ show res ++ ", res);"
                       , "    }"
                       ]
    intVerify r = ("    if res == " ++ show r ++ " {") : print_stdout r
    floatVerify r = ("    if (res - (" ++ show r ++ ")).abs() < 1e-4 {") : print_stdout r
    rustSrc args r = [ "extern \"C\" {"
                     , decl
                     , "    fn ___wasmc__drop();"
                     , "}"
                     , "fn main() {"
                     , "    let res = unsafe { " ++ n ++ "(" ++ argList args ++ ") };"
                     , verify r
                     , "    unsafe { ___wasmc__drop() };"
                     , "}"
                     ]

compileWasm :: B.ByteString -> Either String T.Text
compileWasm = fmap ppllvm . compileModule <=< parseModule

buildTestCase :: Test -> State StdGen [TestTree]
buildTestCase (T testName interface inputOutputPairs wasmSrc) = evalStateT (traverse buildCase stubs) 1
  where
    randomS         = state $ randomR ('a', 'z')
    nStubs          = length stubs
    buildCase stub  = do
      randName <- lift $ replicateM 16 randomS
      n        <- get
      let name | nStubs > 1 = testName ++ "-" ++ show n
               | otherwise  = testName
      modify (+1)
      pure $ testCase name $ assertion (testName ++ "-" ++ randName) stub
    assertion fname stub = do
      wasm    <- either assertFailure pure $ compileWasm wasmSrc
      tmpDir  <- getTemporaryDirectory
      let testDir       = tmpDir ++ "/wasmc_test/"
          f cmd         = cmd { cwd = Just testDir, std_out = CreatePipe, std_err = CreatePipe }
          compileLLCmd  = f $ shell $ "llc-9 --filetype=obj -relocation-model=pic " ++ fname ++ ".ll"
          buildArxivCmd = f $ shell $ "ar -crs lib" ++ fname ++ ".a " ++ fname ++ ".o"
          compileRsCmd  = f $ shell $ "rustc -l" ++ fname ++ " -L " ++ testDir ++ " " ++ fname ++ ".rs"
          testCmd       = f $ shell $ testDir ++ fname
      createDirectoryIfMissing True testDir
      Data.Text.IO.writeFile (testDir ++ fname ++ ".ll") $ T.toStrict wasm
      Data.Text.IO.writeFile (testDir ++ fname ++ ".rs") $ T.toStrict stub
      execCmd compileLLCmd
      execCmd buildArxivCmd
      execCmd compileRsCmd
      stdout <- evalCmd testCmd
      unless (stdout == "passed") $ assertFailure stdout
    stubs         = rustStub interface inputOutputPairs
    execCmd cmd   = evalCmd cmd $> ()
    evalCmd cmd   = do
      (_, stdoutH, stderrH, procH) <- createProcess cmd
      code                         <- waitForProcess procH
      stderr                       <- maybe (pure "") IO.hGetContents stderrH
      unless (code == ExitSuccess) $ assertFailure stderr
      maybe (pure "") IO.hGetContents stdoutH
