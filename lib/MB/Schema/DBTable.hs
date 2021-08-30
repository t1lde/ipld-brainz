{- | Row types for the 'raw' row structure of the tables

-}
module MB.Schema.DBTable () where

--------------------------------------------------------------------------------

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)

--------------------------------------------------------------------------------

import Data.Text qualified as T
import Data.Row (type (.==), type (.+))
import Data.Row.Internal (Row (R),LT ((:->)))

--------------------------------------------------------------------------------

import MB.DB.Types (DBRow, PK, FK, type (:=), type (?=), DBInteger, DBString, DBSmallInt, DBTime, DBBool, UUID)

--------------------------------------------------------------------------------
-- Schema Instances

type instance DBRow "artist" = 'R
  [ "id"               := PK DBInteger
  , "gid"              := UUID
  , "name"             := DBString
  , "sort_name"        := DBString
  , "begin_date_year"  ?= DBSmallInt
  , "begin_date_month" ?= DBSmallInt
  , "begin_date_day"   ?= DBSmallInt
  , "end_date_year"    ?= DBSmallInt
  , "end_date_month"   ?= DBSmallInt
  , "end_date_day"     ?= DBSmallInt
  , "type_id"          ?= (FK "artist_type" "id" DBInteger)
  , "area_id"          ?= (FK "area" "id" DBInteger)
  , "gender_id"        ?= (FK "gender" "id" DBInteger)
  , "comment"          := DBString
  , "edits_pending"    := DBInteger
  , "last_updated"     := DBTime
  , "ended"            := DBBool
  , "begin_area_id"    ?= (FK "area" "id" DBInteger)
  , "end_area_id"      ?= (FK "area" "id" DBInteger)
  ]
