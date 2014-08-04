module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe
import Data.List (isInfixOf)
import Statistics.Regression (olsRegress)
import Data.Vector.Unboxed (fromList)

import Amount
import BatteryUsage
import Phone
import DischargeModel

getSessionUsage :: MaybeT IO [SessionUsage]
getSessionUsage = do
	filename <- MaybeT (Just . head <$> getArgs)
	json <- MaybeT (Just <$> readFile filename)
	MaybeT . return . decodeString $ json

getNotes :: [SessionUsage] -> [String]
getNotes = map note

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delimiter ls 
	| not $ [delimiter] `isInfixOf` ls = [ls]
	| otherwise = first : split delimiter rest where
		(first, d:rest) = break (== delimiter) ls

---------------------------------------------------------
-- Handle duration --------------------------------------
---------------------------------------------------------
parseDuration :: String -> [Amount Int]
parseDuration s = map tupleToAmount $ zip (map read . split ':' $ s) [Hours, Minutes, Seconds]

tupleToAmount :: (a, Unit) -> Amount a
tupleToAmount (x, s1) = Amount x s1 

getDuration :: [SessionUsage] -> [Maybe [Amount Int]]
getDuration = map (fmap parseDuration . duration)

getHours :: Maybe [Int] -> Maybe Int
getHours Nothing = Nothing
getHours (Just x) = Just $ head x

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe Nothing b = b
addMaybe (Just a) b = fmap (+ a) b

getTotalHours :: [Maybe [Int]] -> Maybe Int
getTotalHours = foldr addMaybe (Just 0) . map getHours

durationToSeconds :: [Int] -> Int
durationToSeconds xs = foldr (+) 0 (zipWith (*) sixties $ reverse xs) where
	sixties = iterate (*60) 1 

---------------------------------------------------------
-- main -------------------------------------------------
---------------------------------------------------------
main :: IO ()
main = do
	Just duration <- runMaybeT (getDuration <$> getSessionUsage)
	Just discharge <- runMaybeT ((map getDischarge) <$> getSessionUsage)
	print duration
	print discharge
