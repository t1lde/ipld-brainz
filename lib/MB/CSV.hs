module MB.CSV (runApp, ) where

--------------------------------------------------------------------------------

import Prelude
import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Control.Arrow ((>>>))

--------------------------------------------------------------------------------

import Conduit
import Data.Conduit.Combinators qualified as Cond
import Data.Row (Rec)
import Data.CSV.Conduit (CSVSettings (..), intoCSV, Row)
import Data.Vector qualified as V
import Data.Text qualified as T
import Data.ByteString qualified as B
import System.FilePath.Posix ((</>))
import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

import MB.DB.ToDB (FieldErr (..), ParseTable (..))
import MB.DB.Types (DBRow)

--------------------------------------------------------------------------------

settings :: CSVSettings
settings =
  CSVSettings
    { csvSep = '\t'
    , csvQuoteChar = Nothing
    }

infixr 4 ~>
type (~>) i o = App i o ()
-- | Main App monad stack
type App i o r = ConduitT i o (ExceptT FieldErr (ResourceT IO)) r

-- | Run the application as IO
runApp :: App () Void a -> IO a
runApp =
  runConduit >>> runExceptT >>> runResourceT >>> (>>= handleErr)

handleErr :: (MonadIO m) => Either FieldErr a -> m a
handleErr = either (show >>> putStrLn >>> (*> exitFailure) >>> liftIO) pure

--------------------------------------------------------------------------------

-- | Stream file to ByteString
readDBFile :: FilePath -> FilePath -> (forall i. i ~> B.ByteString)
readDBFile baseDir file =
  sourceFile (baseDir </> file)

-- | Convert to csv text rows
csvRows :: B.ByteString ~> Row T.Text
csvRows = intoCSV settings

-- | Convert to table
toTable ::
  forall (table :: Symbol) (str :: Type). (ParseTable table str) =>
  (Row str) ~> (Rec (DBRow table))
toTable =
  Cond.mapM (parseTable @table @str)

app :: FilePath -> FilePath -> IO ()
app base file = runApp $
  readDBFile base file
    .| csvRows
    .| toTable @"artist" @T.Text
    .| takeC 100
    .| printC
