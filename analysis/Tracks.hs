module Main where

import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.List (subsequences, isInfixOf, delete)

import BatteryUsage

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
parseDuration :: String -> [Int]
parseDuration = map read . split ':'

getDuration :: [SessionUsage] -> [Maybe [Int]]
getDuration = map (fmap parseDuration . duration)

getHours :: Maybe [Int] -> Maybe Int
getHours Nothing = Nothing
getHours (Just x) = Just $ head x

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe Nothing b = b
addMaybe (Just a) b = fmap (+ a) b

getTotalHours :: [Maybe [Int]] -> Maybe Int
getTotalHours = foldr addMaybe (Just 0) . map getHours

---------------------------------------------------------
-- main -------------------------------------------------
---------------------------------------------------------
main :: IO ()
main = do
	Just j <- runMaybeT ((getTotalHours . getDuration) <$> getSessionUsage)
	print j
