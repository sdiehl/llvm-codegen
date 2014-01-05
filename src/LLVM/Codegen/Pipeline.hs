module LLVM.Codegen.Pipeline (
  Pipeline,
  Stage,
  runPipeline,

  Settings(..),
  defaultSettings,

  optimizePass,
  verifyPass,
  showPass,
  showAsmPass
) where

import Data.Word

import Control.Monad.Error

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

type Ctx = (Context, Module, Settings)
type Stage = Ctx -> IO (Either String Ctx)
type Pipeline = [Stage]

data Settings = Settings
    { opt :: Word             -- ^ Optimization level
    , inlineThreshold :: Int  -- ^ Inliner threshold
    } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings { opt = 3, inlineThreshold = 1000 }

-- Compose stages into a composite Stage.
compose :: Stage -> Stage -> Stage
compose a b x = a x >>= either (return . Left) b

-- | Run the verification pass.
verifyPass :: Stage
verifyPass (ctx, m, settings) = do
  putStrLn "Verifying Module..."
  withPassManager defaultCuratedPassSetSpec $ \pm -> do
    valid <- runErrorT $ verify m
    case valid of
      Left err -> throwError (strMsg $ "No verify: " ++ err)
      Right x -> return $ Right (ctx, m, settings)

-- | Dump the generated IR to stdout.
showPass :: Stage
showPass (ctx, m, settings) = do
  putStrLn "Showing Module..."
  s <- moduleString m
  putStrLn s
  return $ Right (ctx, m, settings)

-- | Dump the generated native assembly to stdout.
showAsmPass :: Stage
showAsmPass (ctx, m, settings) = do
  putStrLn "Showing Assembly..."
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

-- | Run the curated pass with the 'opt' level specified in the Settings.
-- @
-- optimizePass 3
-- @
optimizePass :: Word -> Stage
optimizePass level (ctx, m, settings) = do
  putStrLn "Running optimizer..."
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
      -- fold the the list of Stages into a single function, sequentially from left to right
      let res = foldl1 compose pline
      final <- res (ctx, m, settings)
      case final of
        Left err -> throwError (strMsg err)
        Right x -> moduleAST m
