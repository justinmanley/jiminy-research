{-# LANGUAGE OverloadedStrings #-}

module Amount where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson ((.:), decode, eitherDecode, FromJSON(..), Value(..))

------------------------------------------------------
-- Amount --------------------------------------------
------------------------------------------------------
data Amount a = Amount a Unit deriving (Show)

instance (FromJSON a) => FromJSON (Amount a) where
    parseJSON (Object v) =
        Amount <$>
        (v .: "amount") <*>
        (v .: "units")

toMaybeAmount :: Maybe a -> Unit -> Maybe (Amount a)
toMaybeAmount Nothing _ = Nothing
toMaybeAmount (Just x) u = Just $ Amount x u

------------------------------------------------------
-- Unit ----------------------------------------------
------------------------------------------------------
data Unit = Percent 
			| Bytes 
			| Kilobytes 
			| Megabytes 
			| Hours
			| Minutes
			| Seconds
			| MilliAmpereHours deriving (Show, Eq)

instance FromJSON Unit where
	parseJSON (String s) = do
		case s of
			"KB" -> return Kilobytes
			"MB" -> return Megabytes
			"percent" -> return Percent