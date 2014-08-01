module Main where

import Data.Maybe (maybeToList)
import Data.Aeson (decode)
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import BatteryUsage

getNotes :: [SessionUsage] -> [String]
getNotes = map note



--main :: IO (Maybe [SessionUsage])
main = do 
	let filename = ((head <$> getArgs) >>= readFile)
	let notes = decodeString <$> filename 
	n <- notes
	fmap ((mapM_ putStrLn) . getNotes) n 