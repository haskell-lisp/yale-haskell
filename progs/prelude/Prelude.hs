-- Standard value bindings

module Prelude (
    PreludeCore.., PreludeRatio.., PreludeComplex.., PreludeList..,
    PreludeArray.., PreludeText.., PreludeIO.., 
    nullBin, isNullBin, appendBin,
    (&&), (||), not, otherwise,
    minChar, maxChar, ord, chr, 
    isAscii, isControl, isPrint, isSpace, 
    isUpper, isLower, isAlpha, isDigit, isAlphanum,
    toUpper, toLower,
    minInt, maxInt, subtract, gcd, lcm, (^), (^^), 
    fromIntegral, fromRealFrac, atan2,
    fst, snd, id, const, (.), flip, ($), until, asTypeOf, error ) where

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

import PreludePrims

import PreludeCore
import PreludeList
import PreludeArray
import PreludeRatio
import PreludeComplex
import PreludeText
import PreludeIO

infixr 9  .
infixr 8  ^, ^^
infixr 3  &&
infixr 2  ||
infixr 0  $


-- Binary functions

nullBin	    	    	:: Bin
nullBin	    	    	=  primNullBin

isNullBin    	    	:: Bin -> Bool
isNullBin    	    	=  primIsNullBin

appendBin		:: Bin -> Bin -> Bin
appendBin		=  primAppendBin

-- Boolean functions

(&&), (||)		:: Bool -> Bool -> Bool
True  && x		=  x
False && _		=  False
True  || _		=  True
False || x		=  x

not			:: Bool -> Bool
not True		=  False
not False		=  True

{-# (&&)  :: Inline #-}
{-# (||)  :: Inline #-}
{-# not  :: Inline #-}


otherwise		:: Bool
otherwise 		=  True

-- Character functions

minChar, maxChar	:: Char
minChar			= '\0'
maxChar			= '\255'

ord			:: Char -> Int
ord 			=  primCharToInt

chr 			:: Int -> Char
chr 			=  primIntToChar

isAscii, isControl, isPrint, isSpace		:: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphanum	:: Char -> Bool

isAscii c	 	=  ord c < 128
isControl c		=  c < ' ' || c == '\DEL'
isPrint c		=  c >= ' ' && c <= '~'
isSpace c		=  c == ' ' || c == '\t' || c == '\n' || 
			   c == '\r' || c == '\f' || c == '\v'
isUpper c		=  c >= 'A' && c <= 'Z'
isLower c		=  c >= 'a' && c <= 'z'
isAlpha c		=  isUpper c || isLower c
isDigit c		=  c >= '0' && c <= '9'
isAlphanum c		=  isAlpha c || isDigit c


toUpper, toLower	:: Char -> Char
toUpper c | isLower c	= chr ((ord c - ord 'a') + ord 'A')
	  | otherwise	= c

toLower c | isUpper c	= chr ((ord c - ord 'A') + ord 'a')
	  | otherwise	= c

-- Numeric functions

minInt, maxInt	:: Int
minInt		=  primMinInt
maxInt		=  primMaxInt

subtract	:: (Num a) => a -> a -> a
subtract	=  flip (-)

gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "gcd{Prelude}: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x 0  =  x
			 gcd' x y  =  gcd' y (x `rem` y)

lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ (n+1)	=  f x n x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | even n  = g (x*x) (n `quot` 2)
				         | otherwise = f x (n-1) (x*y)
_ ^ _		= error "(^){Prelude}: negative exponent"

(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(-n))

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational

atan2		:: (RealFloat a) => a -> a -> a
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0
			( 1, 0) ->  pi/2
			( 0,-1) ->  pi
			(-1, 0) -> -pi/2
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			( 0, 0) ->  error "atan2{Prelude}: atan2 of origin"


-- Some standard functions:
-- component projections for pairs:
fst			:: (a,b) -> a
fst (x,y)		=  x

snd			:: (a,b) -> b
snd (x,y)		=  y

-- identity function
id			:: a -> a
id x			=  x

-- constant function
const			:: a -> b -> a
const x _		=  x

-- function composition
(.)			:: (b -> c) -> (a -> b) -> a -> c
f . g			=  \ x -> f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x

-- right-associating infix application operator (useful in continuation-
-- passing style)
($)			:: (a -> b) -> a -> b
f $ x			=  f x

-- until p f  yields the result of applying f until p holds.
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf		:: a -> a -> a
asTypeOf		=  const
