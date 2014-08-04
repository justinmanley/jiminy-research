module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe
import Statistics.Regression (olsRegress)
import Data.Vector.Unboxed (fromList)
import Data.Maybe (isJust)

import BatteryUsage
import Phone
import DischargeModel

getSessionUsage :: MaybeT IO [SessionUsage]
getSessionUsage = do
	filename <- MaybeT (Just . head <$> getArgs)
	json <- MaybeT (Just <$> readFile filename)
	MaybeT . return . decodeString $ json

sanitizeSessionUsage :: Maybe [SessionUsage] -> [SessionUsage]
sanitizeSessionUsage Nothing = []
sanitizeSessionUsage (Just xs) = filterSessionUsage xs where
	filterSessionUsage = filterBatteryUsage . filterDataUsage . filterDuration

	filterBatteryUsage = filter hasBatteryUsage where
		hasBatteryUsage x = 
			(isJust . start . batteryUsage $ x) &&
			(isJust . end . batteryUsage $ x) &&
			(isJust . usage . batteryUsage $ x) 	

	filterDataUsage = filter hasDataUsage where
		hasDataUsage = (== 2) . length . dataUsage

	filterDuration = filter hasDuration where
		hasDuration = isJust . duration

---------------------------------------------------------
-- main -------------------------------------------------
---------------------------------------------------------
main :: IO ()
main = do
	s <- sanitizeSessionUsage <$> (runMaybeT getSessionUsage)
	print $ constructPredictors s

