module LLVM.Codegen.Structure (
  record,
  idxOf,
  Fields,
  Record(..)
) where

import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.General.AST (Name(..), Type)

-- | Association list of field names to types.
type Fields = [(Name, Type)]

type FieldsGep = [(Name, Int)]

-- | A level of abstraction on top of the LLVM struct object that carries with it the field names and indexes.
data Record = Record
    { recName   :: String
    , recType   :: Type
    , recFields :: FieldsGep
    } deriving (Eq, Ord, Show)

-- | Lookup the element pointer associated with the field name, for use with GetElementPtr.
idxOf :: Name -> Record -> Maybe Int
idxOf field rec = lookup field (recFields rec)

-- | Construct a record ( underlying is a LLVM struct ) and return the
record :: String -> Fields -> LLVM Record
record name fields = do
  let ty    = struct (map snd fields)
      fgeps = zip (map fst fields) [0..]
  typedef name ty
  return $ Record name ty fgeps
