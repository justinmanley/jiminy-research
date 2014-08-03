{-# LANGUAGE OverloadedStrings #-}

module Amount where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson ((.:), decode, eitherDecode, FromJSON(..), Value(..))

------------------------------------------------------
-- Amount --------------------------------------------
------------------------------------------------------
data Amount a = Amount (Maybe a) Unit deriving (Show)

instance (FromJSON a) => FromJSON (Amount a) where
    parseJSON (Object v) =
        Amount <$>
        (v .: "amount") <*>
        (v .: "units")

------------------------------------------------------
-- Unit ----------------------------------------------
------------------------------------------------------
data Unit = Percent | Bytes | Kilobytes | Megabytes | MilliAmpereHour deriving (Show, Eq)

instance FromJSON Unit where
	parseJSON (String s) = do
		case s of
			"KB" -> return Kilobytes
			"MB" -> return Megabytes
			"percent" -> return Percent