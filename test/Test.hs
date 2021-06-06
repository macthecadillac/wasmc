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
      tests = [binops]

binops :: State StdGen TestTree
binops = testGroup "binop" <$> testCases
  where
    testCases = traverse (uncurry buildTestCase)
              $ zip ["test" ++ show i | i <- [1..]] tests
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

rustStub :: Interface -> [Val] -> Val -> T.Text
rustStub (Interface n a r) args res = T.pack $ intercalate "\n" rustSrc
  where
    argList  = intercalate ", " $ show <$> args
    argTypes = intercalate ", " ["arg" ++ show i ++ ": " ++ show t | (i, t) <- zip [0..] a]
    retType  = maybe "" ((" -> " ++) . show) r
    decl     = "    fn " ++ "func0(" ++ argTypes ++ ")" ++ retType ++ ";"
    verify   = case r of
                 Just I32 -> intercalate "\n" intVerify
                 Just I64 -> intercalate "\n" intVerify
                 Just F32 -> intercalate "\n" floatVerify
                 Just F64 -> intercalate "\n" floatVerify
                 Nothing  -> ""
    print_stdout = [ "        print!(\"passed\");"
                   , "    } else {"
                   , "        print!(\"expected {}, got {}\", " ++ show res ++ ", res);"
                   , "    }"
                   ]
    intVerify = ("    if res == " ++ show res ++ " {") : print_stdout
    floatVerify = ("    if (res - (" ++ show res ++ ")).abs() < 1e-4 {") : print_stdout
    rustSrc  = [ "extern \"C\" {"
               , decl
               , "}"
               , "fn main() {"
               , "    let res = unsafe { func0(" ++ argList ++ ") };"
               , verify
               , "}"
               ]

compileWasm :: B.ByteString -> Either String T.Text
compileWasm = fmap ppllvm . compileModule <=< parseModule

buildTestCase :: String -> Test -> State StdGen TestTree
buildTestCase testName (T interface args res wasmSrc) = do
  fname <- replicateM 16 randomS
  pure $ testCase testName $ assertion fname
    where
      randomS         = state $ randomR ('a', 'z')
      assertion fname = do
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
      stub          = rustStub interface args res
      execCmd cmd   = evalCmd cmd $> ()
      evalCmd cmd   = do
        (_, stdoutH, stderrH, procH) <- createProcess cmd
        code                         <- waitForProcess procH
        stderr                       <- maybe (pure "") IO.hGetContents stderrH
        unless (code == ExitSuccess) $ assertFailure stderr
        maybe (pure "") IO.hGetContents stdoutH
