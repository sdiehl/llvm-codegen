{-# LANGUAGE OverloadedStrings #-}

module LLVM.Codegen.Structure (
  Fields,
  RecordType,
  RecField,
  Record(..),

  record,
  idxOf,
  fieldsOf,
  allocaRecord,
) where

import Data.String

import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Builder
import LLVM.General.AST (Name(..), Type, Operand)

-- | Association list of field names to types.
type Fields = [(RecField, Type)]

-- | A level of abstraction on top of the LLVM struct object that also carries field names and indexes.
data RecordType = RecordType
    { recName   :: String
    , recLLType   :: Type
    , recFields :: [(RecField, Int)] -- Indexes for fields
    } deriving (Eq, Ord, Show)
-- XXX don't export fields!

data Record = Record
    { recValue :: Operand
    , recType :: RecordType
    } deriving (Eq, Ord, Show)

-- Statically enforce the RecFields be unique types, only be used in projection and not as arbitrary string.
newtype RecField = RecField { unRecField :: String }
  deriving (Eq, Ord, Show)

instance IsString RecField where
  fromString = RecField

-- | Lookup the element pointer associated with the field name, for use with GetElementPtr.
idxOf :: RecField -> Record -> Maybe Int
idxOf field rec = lookup field (recFields (recType rec))

fieldsOf :: RecordType -> [RecField]
fieldsOf = map fst . recFields

-- | Construct a record ( underlying is a LLVM struct ) and return the field value.
record :: String -> Fields -> LLVM RecordType
record name fields = do
  let ty    = struct (map snd fields)
      fgeps = zip (map fst fields) [0..]
  typedef name ty
  return $ RecordType name ty fgeps

-- | Stack allocate a record.
allocaRecord :: RecordType -> Codegen Record
allocaRecord ty = do
  val <- alloca (recLLType ty)
  return $ Record { recValue = val, recType = ty }
