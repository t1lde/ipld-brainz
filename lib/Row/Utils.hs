module Row.Utils
  (MapConstRow, MapConst, UnCons) where

--------------------------------------------------------------------------------

import Prelude
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)
import Control.Monad (MonadPlus (..))

--------------------------------------------------------------------------------

import Data.Row.Records
import Data.Row.Internal

--------------------------------------------------------------------------------

-- | Map type-level Const function over a Row
type family MapConstRow (c :: Type) (row :: Row Type) :: Row Type where
  MapConstRow c ('R xs) =  'R (MapConst c xs)

-- | Map type-level Const function over a List
type family MapConst (c :: Type) (row :: [LT Type]) ::  [LT Type] where
  MapConst c '[] = '[]
  MapConst c ((name ':-> x) ': xs) = (name ':-> c) ': (MapConst c xs)

type family UnCons (r :: Row Type) (name :: Symbol) (h :: Type) (t :: Row Type) :: Constraint where
  UnCons ('R ((name ':-> h) ': ts)) name h ('R ts) = ()
