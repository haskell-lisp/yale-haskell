{-****************************************************************
   MODULE R_CONSTANTS

     This module sets up all the constants used in this functional
   animation package.
     Defined here are the basic units of movement, scale and rotation.
   The screen height and width are set, and the various parts of
   the screen such as the top-middle, lower-left and center are
   all set. Finally the color values used by xcshow, the c-program
   that displays the movies in X, are set.

******************************************************************-}

module R_Constants (fps, unit, hf, qt, scaleunit, rotunit,
                    nullpic, nullseq,
                    sinunit,cosunit,
                    screenwid, screenht, botl, leftm, topl, topm, topr,
                    rightm, botr, botm, center,
		    white,black,red,green,darkblue,lightblue,brown,yellow,
		    colorName, allColors
			) where

import R_Ptypes

  -- units are set. The scaleunit is in 11th, so that the 12 is
  -- actually 12/11'ths
fps :: Int
unit :: Int
hf :: Int
qt :: Int
scaleunit :: Int
fps = 25
unit = 15
hf =  unit `div` 2
qt =  unit `div`4
scaleunit = 12
  --scaleunit is div'ed by 12 later

rotunit :: Float
rotunit  = pi/18
sinunit  = sin rotunit
cosunit  = cos rotunit


nullpic :: Pic
nullpic = []
nullseq :: Movie
nullseq= nullpic : [ nullseq2 | nullseq2 <- nullseq]

  -- Screen Parameters
screenwid :: Int
screenwid = 800
screenht :: Int
screenht  = 800

botl :: Vec
leftm :: Vec
topl :: Vec
topm :: Vec
topr :: Vec
rightm :: Vec
botr :: Vec
botm :: Vec
center :: Vec

leftmb :: Vec
leftmt :: Vec
topml :: Vec
topmr :: Vec
rightmt :: Vec
rightmb :: Vec
botml :: Vec
botmr :: Vec

botl   = ( 0, 0 )
leftm  = ( 0, screenht `div` 2)
topl   = ( 0, screenht )
topm   = ( screenwid `div` 2, screenht )
topr   = ( screenwid, screenht )
rightm = ( screenwid, screenht `div` 2 )
botr   = ( screenwid, 0 )
botm   = ( screenwid `div` 2, 0 )
center = ( screenwid `div` 2, screenht `div` 2 )

leftmb  = ( 0, screenht `div` 4 )
leftmt  = ( 0, (screenht*3) `div` 4 )
topml   = ( screenwid `div` 4, screenht )
topmr   = ( (screenwid*3) `div` 4, screenht )
rightmt = ( screenwid, (screenht*3) `div` 4 )
rightmb = ( screenwid, screenht `div` 4 )
botml   = ( screenwid `div` 4, 0 )
botmr   = ( (screenwid*3) `div` 4, 0 )

  -- Colors values set to names

white :: Color
white = 1
black :: Color
black = 2
red :: Color
red = 4
green :: Color
green = 6
darkblue :: Color
darkblue = 8
lightblue :: Color
lightblue = 10
yellow :: Color
yellow = 12
brown :: Color
brown = 14

colorName :: Color -> String
colorName 1 = "white"
colorName 2 = "black"
colorName 4 = "red"
colorName 6 = "green"
colorName 8 = "blue"
colorName 10 = "lightblue"
colorName 12 = "yellow"
colorName 14 = "brown"

allColors :: [Color]
allColors = [1,2,4,6,8,10,12,14]





