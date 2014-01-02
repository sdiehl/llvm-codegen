module LLVM.Codegen.Structure (
  record,
  idxOf,
  Fields,
  RecordType(..)
) where

import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.General.AST (Name(..), Type)

-- | Association list of field names to types.
type Fields = [(Name, Type)]
type FieldsIdx = [(Name, Int)]

-- | A level of abstraction on top of the LLVM struct object that also carries field names and indexes.
data RecordType = RecordType
    { recName   :: String
    , recType   :: Type
    , recFields :: FieldsIdx
    } deriving (Eq, Ord, Show)

-- | Lookup the element pointer associated with the field name, for use with GetElementPtr.
idxOf :: Name -> RecordType -> Maybe Int
idxOf field rec = lookup field (recFields rec)

-- | Construct a record ( underlying is a LLVM struct ) and return the
record :: String -> Fields -> LLVM RecordType
record name fields = do
  let ty    = struct (map snd fields)
      fgeps = zip (map fst fields) [0..]
  typedef name ty
  return $ RecordType name ty fgeps
