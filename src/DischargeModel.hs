module DischargeModel where

import Amount
import Phone
import BatteryUsage

---------------------------------------------------------
-- model ------------------------------------------------
---------------------------------------------------------


subtractMaybeAmount :: Num a => Maybe (Amount a) -> Maybe (Amount a) -> Maybe (Amount a)
subtractMaybeAmount Nothing _ = Nothing
subtractMaybeAmount _ Nothing = Nothing
subtractMaybeAmount (Just (Amount x s1)) (Just (Amount y s2))
	| s1 == s2 = Just $ Amount (x - y) s1
	| otherwise = Nothing

startTime :: SessionUsage -> Maybe (Amount Int)
startTime = start . batteryUsage

endTime :: SessionUsage -> Maybe (Amount Int)
endTime = end . batteryUsage

getDischarge :: SessionUsage -> Maybe (Amount Int)
getDischarge u = subtractMaybeAmount (startTime u) (endTime u)

--constructPredictors :: [SessionUsage] -> [Vector]
--constructPredictors xs = [fromList v | [getDuration x, getDischarge x] <- xs ] 

--modelBatteryDischarge :: [SessionUsage] -> (Phone -> [Int])