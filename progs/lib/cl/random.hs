-- random.hs -- random number functions
--
-- author :  Sandra Loosemore
-- date   :  22 June 1993
--

module Random where

import RandomPrims  -- from random-prims.hi

class RandomOperand a where
  random	:: a -> IO a

instance RandomOperand Int where
  random	= randomInt
instance RandomOperand Integer where
  random	= randomInteger
instance RandomOperand Float where
  random	= randomFloat
instance RandomOperand Double where
  random	= randomDouble
