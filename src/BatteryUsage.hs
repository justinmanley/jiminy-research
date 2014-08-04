{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BatteryUsage where

import Data.Maybe (maybeToList)
import Data.Aeson ((.:), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import GHC.Generics
import Data.List (isInfixOf)


decodeString :: FromJSON a => String -> Maybe a
decodeString = decode . ByteString.pack

------------------------------------------------------
-- SessionUsage: Top level data structure ------------
------------------------------------------------------
data SessionUsage = SessionUsage {
	note :: String,
	date :: Date,
	appName :: String,
	duration :: Maybe [Int],
	batteryUsage :: BatteryUsage,
	dataUsage :: [DataUsage]
} deriving (Show)

-- | Note below that fmap is applied twice: once as fmap, and once as <$>.
--   The first application of fmap gets parseDuration into the Parser monad.
--   The second application of fmap gets parseDuration into the Maybe monad.
instance FromJSON SessionUsage where
	parseJSON (Object v) = 
		SessionUsage <$> 
		(v .: "note") <*>
		(v .: "date") <*>
		(v .: "app") <*>
		(fmap parseDuration <$> v .: "duration") <*>
		(v .: "battery-usage") <*>
		(v .: "data-usage")

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delimiter ls 
	| not $ [delimiter] `isInfixOf` ls = [ls]
	| otherwise = first : split delimiter rest where
		(first, d:rest) = break (== delimiter) ls

parseDuration :: String -> [Int]
parseDuration = map read . split ':'

------------------------------------------------------
-- BatteryUsage --------------------------------------
------------------------------------------------------
data BatteryUsage = BatteryUsage {
	start :: Maybe Int,
	end :: Maybe Int,
	usage :: Maybe Int
} deriving (Show)

instance FromJSON BatteryUsage where
	parseJSON (Object v) = 
		BatteryUsage <$>
		(v .: "start") <*>
		(v .: "end") <*>
		((v .: "app") >>= (.: "amount"))

------------------------------------------------------
-- DataUsage -----------------------------------------
------------------------------------------------------
data DataUsage = DataUsage {
	usageType :: String,
	sent :: Int, 
	received :: Int
} deriving (Show)

instance FromJSON DataUsage where
	parseJSON (Object v) = 
		DataUsage <$>
		(v .: "type") <*>
		((v .: "sent") >>= (.: "amount")) <*>
		((v .: "received") >>= (.: "amount"))

------------------------------------------------------
-- Date ----------------------------------------------
------------------------------------------------------
data Date = Date {
	year :: Int,
	month :: String,
	day :: Int
} deriving (Show, Generic)

instance FromJSON Date