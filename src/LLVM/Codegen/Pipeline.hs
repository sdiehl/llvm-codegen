module LLVM.Codegen.Pipeline (
  Pipeline,
  Stage,
  runPipeline,

  Settings(..),
  defaultSettings,

  optimizePass,
  verifyPass,
  showPass
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
    { opt :: Word
    } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings { opt = 3 }

compose :: Stage -> Stage -> Stage
compose a b x = a x >>= either (return . Left) b

verifyPass :: Stage
verifyPass (ctx, m, settings) = do
  putStrLn "Verifying Module..."
  withPassManager defaultCuratedPassSetSpec $ \pm -> do
    valid <- runErrorT $ verify m
    case valid of
      Left err -> throwError (strMsg $ "No verify: " ++ err)
      Right x -> return $ Right (ctx, m, settings)

showPass :: Stage
showPass (ctx, m, settings) = do
  putStrLn "Showing Module..."
  s <- moduleString m
  putStrLn s
  return $ Right (ctx, m, settings)

optimizePass :: Stage
optimizePass (ctx, m, settings) = do
  putStrLn "Running optimizer..."
  let passes = defaultCuratedPassSetSpec { optLevel = Just (opt settings) }
  withPassManager passes $ \pm -> do
    runPassManager pm m
  return $ Right (ctx, m, settings)

-- | Run a AST through the compiler pipeline executing arbitrary effects specified by the pipeline, and
-- finally either returning the resulting AST or the CompilerError.
runPipeline :: Pipeline -> Settings -> AST.Module -> IO (Either String AST.Module)
runPipeline pline settings ast = do
  withContext $ \ctx ->
    runErrorT $ withModuleFromAST ctx ast $ \m -> do
      let res = foldl1 compose pline
      final <- res (ctx, m, settings)
      case final of
        Left err -> throwError (strMsg err)
        Right x -> moduleAST m
