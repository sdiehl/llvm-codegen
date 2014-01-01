module LLVM.Codegen.NameSupply (
  Names,
  uniqueName
) where

import qualified Data.Map as Map

-- | Collision free name supply.
type Names = Map.Map String Int

-- | Returns either a unique name or a mangled name within the given name supply.
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)
