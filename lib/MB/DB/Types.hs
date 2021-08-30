{-
  This module defines a set of core types approximating the PostgresQL primitives
    needed to define the MusicBrainz schema.

  The amount of parsing from the UTF-8 data input is kept relatively minimal, as we
   parse the real structure out of this data later.

-}
module MB.DB.Types
  ( DBBool (..)
  , DBInteger (..)
  , DBString (..)
  , UUID (..)
  , DBSmallInt (..)
  , DBTime (..)
  , Nullable (..)
  , PK (..)
  , FK (..)
  , DBRow
  , DBRowRaw
  , type (?=)
  , type (:=)
  )
  where

--------------------------------------------------------------------------------

import Prelude hiding (null)
import Data.String (IsString)
import GHC.TypeLits (Symbol)
import Data.Int (Int32, Int16)
import Data.Coerce (coerce)
import Data.Kind (Type)

--------------------------------------------------------------------------------

import Data.Text qualified as T
import Data.Row.Internal (LT ((:->)), Row)
import Data.Text.Read qualified as T
import Data.UUID qualified as U
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)

--------------------------------------------------------------------------------

import Row.Utils (MapConstRow)

--------------------------------------------------------------------------------
-- | Open type family looking up table types by a symbol name
type family DBRow (a :: Symbol) :: Row Type

-- | Get the 'Raw' unparsed row by replacing all the types in a row with Text
type family DBRowRaw (table :: Symbol) (str :: Type) :: Row Type where
  DBRowRaw table str = MapConstRow str (DBRow table)

infix 7 :=
infix 7 ?=
type name := ty = name ':-> ty
type name ?= ty = name ':-> ty

--------------------------------------------------------------------------------
-- Base Types

-- | Booleans
newtype DBBool = DBBool Bool
  deriving newtype (Show, Ord, Eq, Enum)

-- | Integer: 4 Bytes
newtype DBInteger = DBInteger Int32
  deriving newtype (Show, Ord, Eq, Bounded, Enum, Num)

-- | String: Unicode Text
newtype DBString = DBString T.Text
  deriving newtype (Show, Ord, Eq, IsString)

-- | UUIDs: see 'U.UUID'
newtype UUID = UUID U.UUID
  deriving newtype (Show, Ord, Eq)

-- | SmallInteger: 2 Bytes
newtype DBSmallInt = DBSmallInt Int16
  deriving newtype (Show, Ord, Eq, Bounded, Enum, Num)

-- | UTCTime. In the DB dump this appears to be ISO8601 formatted time.
newtype DBTime = DBTime Time.UTCTime
  deriving newtype (Ord, Eq)

--------------------------------------------------------------------------------
-- Composite types

-- | Nullable: Stricter version of 'Maybe'.
data Nullable a
  = Some !a
  | Null
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Types with a special DB meaning

-- | Tag a column as a PrimaryKey for the table
newtype PK a = PK a
  deriving (Show, Ord, Eq)

-- | Tag a column as a ForeignKey to another table
newtype FK
  (table :: Symbol)
  (field :: Symbol)
  (a :: Type) = FK a
  deriving (Show, Ord, Eq)

--------------------------------------------------------------------------------
-- base Instances

-- | Showing via ISO8601
instance Show DBTime where
  show = coerce @(Time.UTCTime -> String) @(DBTime -> String) iso8601Show

--------------------------------------------------------------------------------

