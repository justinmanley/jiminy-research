module Phone where

data Phone = Phone {
	battery :: Battery
}

data Battery = Battery {
	capacity :: Int,
	talkTime :: Int,
	standbyTime :: Int
}