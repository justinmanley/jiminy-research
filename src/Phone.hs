module Phone where

import Amount

data Phone = Phone {
	battery :: Battery
}

data Battery = Battery {
	capacity :: Amount Int,
	talkTime :: Amount Int,
	standbyTime :: Amount Int
}