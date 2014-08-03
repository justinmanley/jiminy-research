{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BatteryUsage where

import Data.Maybe (maybeToList)
import Data.Aeson ((.:), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import GHC.Generics

import Amount

decodeString :: FromJSON a => String -> Maybe a
decodeString = decode . ByteString.pack

------------------------------------------------------
-- SessionUsage: Top level data structure ------------
------------------------------------------------------
data SessionUsage = SessionUsage {
	note :: String,
	date :: Date,
	appName :: String,
	duration :: Maybe String,
	batteryUsage :: BatteryUsage,
	dataUsage :: [DataUsage]
} deriving (Show)

instance FromJSON SessionUsage where
	parseJSON (Object v) = 
		SessionUsage <$> 
		(v .: "note") <*>
		(v .: "date") <*>
		(v .: "app") <*>
		(v .: "duration") <*>
		(v .: "battery-usage") <*>
		(v .: "data-usage")

------------------------------------------------------
-- BatteryUsage --------------------------------------
------------------------------------------------------
data BatteryUsage = BatteryUsage {
	start :: Amount Int,
	end :: Amount Int,
	usage :: Amount Int
} deriving (Show)

instance FromJSON BatteryUsage where
	parseJSON (Object v) = do
		start <- v .: "start"
		end <- v .: "end"
		units <- v .: "units"
		usage <- v .: "app"
		return $ BatteryUsage (Amount start units) (Amount end units) usage

------------------------------------------------------
-- DataUsage -----------------------------------------
------------------------------------------------------
data DataUsage = DataUsage {
	usageType :: String,
	sent :: Amount Int, 
	received :: Amount Int
} deriving (Show)

instance FromJSON DataUsage where
	parseJSON (Object v) = 
		DataUsage <$>
		(v .: "type") <*>
		(v .: "sent") <*>
		(v .: "received")

------------------------------------------------------
-- Date ----------------------------------------------
------------------------------------------------------
data Date = Date {
	year :: Int,
	month :: String,
	day :: Int
} deriving (Show, Generic)

instance FromJSON Date