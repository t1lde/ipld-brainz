module MB.DB.ToDB (ToDB (..), ParseTable (..), FieldErr (..)) where

--------------------------------------------------------------------------------

import Prelude hiding (null)
import Control.Arrow ((>>>))
import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Kind (Type)
import Data.Function ((&))
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Coerce (coerce)
import Data.Int (Int32, Int16)
import Data.String (IsString)
import Data.List (uncons)

--------------------------------------------------------------------------------

import Control.Monad.State.Strict (evalStateT)
import Data.Row (Forall, Rec, Row, Label (..), HasType, AllUniqueLabels, type (.!), (.+), (.==), type (.==), type (.+))
import Data.Row.Internal (Row (R))
import Control.Monad.Except (MonadError (..))
import Data.Row.Records qualified as Rec
import Data.Either.Combinators (rightToMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.UUID qualified as U
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Control.Monad.State.Class (MonadState (..), gets)

--------------------------------------------------------------------------------

import MB.DB.Types
import Row.Utils (UnCons)

--------------------------------------------------------------------------------

-- | ToDB class, managing conversion from some type to a DB type
class ToDB (a :: Type) (db :: Type) where
  toDB :: a -> Maybe db

---------- Base Types

instance ToDB T.Text DBBool where
  toDB b
    | b == "t" = pure $ DBBool True
    | b == "f" = pure $ DBBool False
  toDB _ = Nothing

instance ToDB T.Text DBSmallInt where
  toDB x = do
    (i,_) <- x & T.signed (T.decimal @Integer) & rightToMaybe
    guard (i >= (toInteger $ minBound @Int16 ))
    guard (i <= (toInteger $ maxBound @Int16 ))
    pure $ DBSmallInt $ fromInteger i

instance ToDB T.Text DBInteger where
  toDB x = do
    (i,_) <-
      x & T.signed (T.decimal @Integer) & rightToMaybe
    guard (i >= (toInteger $ minBound @Int32 ))
    guard (i <= (toInteger $ maxBound @Int32 ))
    pure $ DBInteger $ fromInteger i

instance ToDB T.Text DBString where
  toDB = DBString >>> pure

instance ToDB T.Text UUID where
  toDB = fmap UUID . U.fromText

instance ToDB T.Text DBTime where
  toDB =
    T.unpack >>> iso8601ParseM >>> fmap (Time.zonedTimeToUTC >>> DBTime)

---------- Composite Types

instance (ToDB str a, Eq str, IsString str) => ToDB str (Nullable a) where
  toDB x
    =   (Null <$ guard (x == "\\N"))
    <|> (Some <$> (toDB @str @a x))

---------- DB Types
instance (ToDB str a) => ToDB str (PK a) where
  toDB = coerce (toDB @str @a)

instance forall str a table field. (ToDB str a) => ToDB str (FK table field a) where
  toDB = coerce (toDB @str @a)

--------------------------------------------------------------------------------

data FieldErr
  = Missing | Failed
  deriving stock Show

class ParseTable (table :: Symbol) (str :: Type) where
  parseTable ::
    forall (m :: Type -> Type).
    ( MonadError FieldErr m
    ) =>
    [str] -> m (Rec (DBRow table))

instance (ParseRow (DBRow table) str) => ParseTable table str where
  parseTable = parseRow @(DBRow table) @str

class ParseRow (row :: Row Type) (str :: Type) where
  parseRow :: forall (m :: Type -> Type). (MonadError FieldErr m) => [str] -> m (Rec row)

instance
  ( UnCons row name head tail
  , ((name .== head) .+ tail) ~ row
  , (row .! name) ~ head
  , ParseRow tail str
  , KnownSymbol name
  , ToDB str head
  ) =>
  ParseRow row str where
  parseRow (x : xs) = do
    field <-
      toDB @str @head x
        & maybe (throwError Failed) pure
    rest <- parseRow @tail @str xs
    pure $
      (Label @name .== field) .+ rest
  parseRow [] = throwError Missing

instance ParseRow ('R '[]) str where
  parseRow _ = pure Rec.empty
