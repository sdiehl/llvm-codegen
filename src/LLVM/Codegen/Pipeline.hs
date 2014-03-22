module LLVM.Codegen.Pipeline (
  Pipeline,
  Stage,
  Check,
  runPipeline,
  runPipeline_,

  Settings(..),
  defaultSettings,
  ifVerbose,

  -- passes
  condStage,
  optimizeStage,
  verifyStage,
  showStage,
  showAsmStage
) where

import Data.Word
import Data.List

import Control.Monad.Error

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.Analysis
import LLVM.General.Transforms
import LLVM.General.PassManager

type Ctx = (Context, Module, Settings)
type Check = Ctx -> IO Bool
type Stage = Ctx -> IO (Either String Ctx)
type Pipeline = [Stage]

-- Pipeline settings
data Settings = Settings
    { verbose :: Bool         -- ^ Verbosity
    } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings { verbose = False}

-- Compose stages into a composite Stage.
compose :: Stage -> Stage -> Stage
compose a b x = a x >>= either (return . Left) b

-- Run the stage only if verobse is toggled True.
ifVerbose :: Stage -> Stage
ifVerbose pass opts@(ctx, m, settings) =
  if verbose  settings then
    pass opts
  else
    noopStage opts

trace :: Settings -> String -> IO ()
trace settings msg = if verbose settings
  then putStrLn msg
  else return ()

-- | Do nothing pass.
noopStage :: Stage
noopStage = return . Right

-- | Run the verification pass.
verifyStage :: Stage
verifyStage (ctx, m, settings) = do
  trace settings "Verifying Module..."
  withPassManager defaultCuratedPassSetSpec $ \pm -> do
    valid <- runErrorT $ verify m
    case valid of
      Left err -> throwError (strMsg $ "No verify: " ++ err)
      Right x -> return $ Right (ctx, m, settings)

condStage :: (Ctx -> IO Bool) -> Stage
condStage test opts = do
  tval <- test opts
  if tval then
    return $ Right opts
  else
    throwError (strMsg "Failed condition.")

-- | Dump the generated IR to stdout.
showStage :: Stage
showStage (ctx, m, settings) = do
  trace settings "Showing Module..."
  s <- moduleString m
  putStrLn s
  return $ Right (ctx, m, settings)

-- | Dump the generated native assembly to stdout.
showAsmStage :: Stage
showAsmStage (ctx, m, settings) = do
  trace settings "Showing Assembly..."
  asm <- runErrorT $ withDefaultTargetMachine $ \tm -> do
    gen <- runErrorT $ moduleAssembly tm m
    case  gen of
      Left err -> throwError (strMsg "Error building target machine.")
      Right a -> return a
  case asm of
    Left err -> throwError (strMsg "Error generating assembly.")
    Right asm -> do
      putStrLn asm
      return $ Right (ctx, m, settings)

-- | Run the curated pass with the 'opt' level specified.
optimizeStage :: Word -> Stage
optimizeStage level (ctx, m, settings) = do
  trace settings "Running optimizer..."
  let passes = defaultCuratedPassSetSpec { optLevel = Just level }
  withPassManager passes $ \pm -> do
    runPassManager pm m
  return $ Right (ctx, m, settings)

-- | Run a AST through the compiler pipeline executing arbitrary effects specified by the pipeline, and
-- finally either returning the resulting AST or the CompilerError.
runPipeline :: Pipeline -> Settings -> AST.Module -> IO (Either String AST.Module)
runPipeline pline settings ast = do
  withContext $ \ctx ->
    runErrorT $ withModuleFromAST ctx ast $ \m -> do
      -- fold the the list of Stages into a single function,
      -- sequentially from left to right
      let res = foldl1' compose pline
      final <- res (ctx, m, settings)
      case final of
        Left err -> throwError (strMsg err)
        Right x -> moduleAST m

runPipeline_ :: Pipeline -> Settings -> AST.Module -> IO (Either String String)
runPipeline_ pline settings ast = do
  withContext $ \ctx ->
    runErrorT $ withModuleFromAST ctx ast $ \m -> do
      -- fold the the list of Stages into a single function,
      -- sequentially from left to right
      let res = foldl1' compose pline
      final <- res (ctx, m, settings)
      case final of
        Left err -> throwError (strMsg err)
        Right x -> moduleString m
