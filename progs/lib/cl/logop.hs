-- logop.hs -- logical operations on numbers
--
-- author :  Sandra Loosemore
-- date   :  19 June 1993
--

module LogOp where

import LogOpPrims  -- from logop-prims.hi

class LogOperand a where
  logior	:: a -> a -> a
  logxor	:: a -> a -> a
  logand	:: a -> a -> a
  logeqv	:: a -> a -> a
  lognand	:: a -> a -> a
  lognor	:: a -> a -> a
  logandc1	:: a -> a -> a
  logandc2	:: a -> a -> a
  logorc1	:: a -> a -> a
  logorc2	:: a -> a -> a
  lognot	:: a -> a
  logtest	:: a -> a -> a
  logbitp	:: Int -> a -> a
  ash		:: a -> Int -> a
  logcount	:: a -> Int
  integerLength :: a -> Int

instance LogOperand Integer where
  logior	= logiorInteger
  logxor	= logxorInteger
  logand	= logandInteger
  logeqv	= logeqvInteger
  lognand	= lognandInteger
  lognor	= lognorInteger
  logandc1	= logandc1Integer
  logandc2	= logandc2Integer
  logorc1	= logorc1Integer
  logorc2	= logorc2Integer
  lognot	= lognotInteger
  logtest	= logtestInteger
  logbitp	= logbitpInteger
  ash		= ashInteger
  logcount	= logcountInteger
  integerLength	= integerLengthInteger

instance LogOperand Int where
  logior	= logiorInt
  logxor	= logxorInt
  logand	= logandInt
  logeqv	= logeqvInt
  lognand	= lognandInt
  lognor	= lognorInt
  logandc1	= logandc1Int
  logandc2	= logandc2Int
  logorc1	= logorc1Int
  logorc2	= logorc2Int
  lognot	= lognotInt
  logtest	= logtestInt
  logbitp	= logbitpInt
  ash		= ashInt
  logcount	= logcountInt
  integerLength	= integerLengthInt
