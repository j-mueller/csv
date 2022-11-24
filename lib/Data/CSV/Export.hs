{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Exporting data to CSV
module Data.CSV.Export
  ( CSVRow (..),
    writeCSV,
    generateCSV,
    writeCSVHeader,
    writeCSVLine,
    writeCSVStream,
    toHandle,

    -- ** Config
    DecimalPoint (..),
    CSVConfig (..),
    germanCSVConfig,
    defaultCSVConfig,

    -- ** Newtypes
    UTCTimeAsDateTime(..),
    UTCTimeAsDay(..)
  )
where

import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text.IO
import           Data.Time.Clock          (UTCTime (..))
import qualified Data.Time.Format.ISO8601 as F
import           GHC.IO.Handle            (Handle)
import           Streaming                (Of(..))
import           Streaming.Internal       (Stream (..))
import qualified Streaming.Prelude        as S
import qualified System.IO                as IO

{-| Values that can turned into CSV rows.
-}
class CSVRow a where
  headers :: Proxy a -> [Text]
  headers _ = [""]

  toRow :: a -> [Text]

instance CSVRow Int where
  toRow = return . Text.pack . show

instance CSVRow Double where
  toRow = return . Text.pack . show

instance CSVRow Text where
  toRow = return

instance CSVRow String where
  toRow = return . Text.pack

instance (CSVRow a, CSVRow b) => CSVRow (a, b) where
  toRow (l, r) = toRow l <> toRow r
  headers _ = headers (Proxy @a) <> headers (Proxy @b)

instance (CSVRow a, CSVRow b, CSVRow c) => CSVRow (a, b, c) where
  toRow (a, b, c) = toRow a <> toRow b <> toRow c
  headers _ = headers (Proxy @a) <> headers (Proxy @b) <> headers (Proxy @c)

instance (CSVRow a, CSVRow b, CSVRow c, CSVRow d) => CSVRow (a, b, c, d) where
  toRow (a, b, c, d) = toRow a <> toRow b <> toRow c <> toRow d
  headers _ = headers (Proxy @a) <> headers (Proxy @b) <> headers (Proxy @c) <> headers (Proxy @d)

{-| Newtype for 'UTCTime' with a 'CSVRow' instance that just prints out the date
-}
newtype UTCTimeAsDay = UTCTimeAsDay{ getUTCTimeAsDay :: UTCTime }

instance CSVRow UTCTimeAsDay where
  toRow = return . Text.pack . F.iso8601Show . utctDay . getUTCTimeAsDay
  headers _ = ["Day"]

{-| Newtype for 'UTCTime' with a 'CSVRow' instance that prints out day and time
-}
newtype UTCTimeAsDateTime = UTCTimeAsDateTime{ getUTCTimeAsDateTime :: UTCTime }

instance CSVRow UTCTimeAsDateTime where
  toRow = return . Text.pack . F.iso8601Show . getUTCTimeAsDateTime
  headers _ = ["datetime"]

instance CSVRow UTCTime where
  toRow = return . Text.pack .  F.iso8601Show . utctDay
  headers _ = ["Day"]

data DecimalPoint = DecimalComma | DecimalPoint

data CSVConfig
  = CSVConfig
      { csvDelimiter     :: Text,
        csvQuotationMark :: Maybe Text,
        csvDecimalPoint  :: DecimalPoint
      }

processCell :: CSVConfig -> Text -> Text
processCell CSVConfig {csvQuotationMark, csvDecimalPoint} =
  let quote =
        case csvQuotationMark of
          Nothing -> id
          Just c  -> \t -> c <> t <> c
      repl =
        case csvDecimalPoint of
          DecimalPoint -> id
          DecimalComma -> Text.replace "." ","
   in quote . repl

germanCSVConfig :: CSVConfig
germanCSVConfig = CSVConfig {csvDelimiter = ";", csvDecimalPoint = DecimalComma, csvQuotationMark = Just "\""}

defaultCSVConfig :: CSVConfig
defaultCSVConfig = CSVConfig {csvDelimiter = ",", csvDecimalPoint = DecimalPoint, csvQuotationMark = Just "\""}

mkLine :: CSVRow a => CSVConfig -> a -> Text
mkLine config@CSVConfig{csvDelimiter} =
  Text.intercalate csvDelimiter . fmap (processCell config) . toRow

mkHeader :: CSVRow a => CSVConfig -> Proxy a -> Text
mkHeader config@CSVConfig{csvDelimiter} =
  Text.intercalate csvDelimiter . fmap (processCell config) . headers

writeCSVHeader :: forall a. CSVRow a => Handle -> CSVConfig -> Proxy a -> IO ()
writeCSVHeader handle config = Text.IO.hPutStrLn handle . mkHeader config

writeCSVLine :: forall a. CSVRow a => Handle -> CSVConfig -> a -> IO ()
writeCSVLine handle config = Text.IO.hPutStrLn handle . mkLine config

generateCSV ::
  forall a.
  CSVRow a =>
  CSVConfig ->
  [a] ->
  Text
generateCSV config a =
  let CSVConfig {csvDelimiter} = config
      mkLine' = Text.intercalate csvDelimiter . fmap (processCell config)
   in Text.unlines $
        mkLine' (headers (Proxy @a)) : fmap mkLine' (toRow <$> a)

writeCSV ::
  forall a.
  (CSVRow a) =>
  CSVConfig ->
  FilePath ->
  [a] ->
  IO ()
writeCSV config fp = writeCSVStream config fp . S.each

{-| Stream CSV rows into a file
-}
writeCSVStream ::
  forall a.
  (CSVRow a) =>
  CSVConfig ->
  FilePath ->
  Stream (Of a) IO () ->
  IO ()
writeCSVStream config fp a = do
  IO.withFile fp IO.WriteMode $ \handle -> do
    writeCSVHeader handle config (Proxy @a)
    toHandle config handle a

{-| Write a stream of values to a handle as CSV rows
-}
toHandle :: forall a m r. (CSVRow a, MonadIO m) => CSVConfig -> Handle -> Stream (Of a) m r -> m r
toHandle config handle = S.mapM_ k where
  k = liftIO . writeCSVLine handle config
{-# INLINABLE toHandle #-}
