module LLVM.Codegen.Structure (
  Fields,
  Record(..),

  record,
  idxOf,
  fieldsOf,
  allocaRecord,
) where

import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Builder
import LLVM.General.AST (Name(..), Type, Operand)

-- | Association list of field names to types.
type Fields = [(Name, Type)]

-- | A level of abstraction on top of the LLVM struct object that also carries field names and indexes.
data Record = Record
    { recName   :: String
    , recType   :: Type
    , recFields :: [(Name, Int)] -- Indexes for fields
    } deriving (Eq, Ord, Show)

-- | Lookup the element pointer associated with the field name, for use with GetElementPtr.
idxOf :: Name -> Record -> Maybe Int
idxOf field rec = lookup field (recFields rec)

fieldsOf :: Record -> [Name]
fieldsOf = map fst . recFields

-- | Construct a record ( underlying is a LLVM struct ) and return the field value.
record :: String -> Fields -> LLVM Record
record name fields = do
  let ty    = struct (map snd fields)
      fgeps = zip (map fst fields) [0..]
  typedef name ty
  return $ Record name ty fgeps

-- | Stack allocate a record.
allocaRecord :: Record -> Codegen Operand
allocaRecord = alloca . recType
