module DischargeModel where

import Data.Maybe (fromJust)
import Data.Vector.Unboxed (fromList, Vector(..))

import Phone
import BatteryUsage

---------------------------------------------------------
-- Duration ---------------------------------------------
---------------------------------------------------------

getDuration :: SessionUsage -> Int
getDuration = fromJust . (fmap durationToSeconds) . duration 

durationToSeconds :: [Int] -> Int
durationToSeconds xs = foldr (+) 0 (zipWith (*) sixties $ reverse xs) where
	sixties = iterate (*60) 1

---------------------------------------------------------
-- Discharge --------------------------------------------
---------------------------------------------------------

startStateOfCharge :: SessionUsage -> Int
startStateOfCharge = fromJust . start . batteryUsage

endStateOfCharge :: SessionUsage -> Int
endStateOfCharge = fromJust . end . batteryUsage

getDischarge :: SessionUsage -> Int
getDischarge u = startStateOfCharge u - endStateOfCharge u

---------------------------------------------------------
-- model ------------------------------------------------
---------------------------------------------------------

constructPredictors :: [SessionUsage] -> [Vector Int]
constructPredictors = map constructPredictor where
	constructPredictor u = fromList [getDischarge u, getDuration u]

--modelBatteryDischarge :: [SessionUsage] -> (Phone -> [Int])