{-***********************************************************************
   MODULE PTYPES

     This module contains the definitions for all the basic datatypes used to
   create functional movies. 
     The basis of all the images is the Poly, which is a tuple of a color
   and a list of points. This is displayed as a polygon of that color. The 
   form is a line drawn to each of the points, in order.
     A list of these Poly's is a Pic, or picture. Each picture is a single
   frame of the movie. A list of Pic's makes up a Movie, which is a series
   of Pic's displayed in order.
     Behaviours affect the movies, such as moving them left, or right.
     PictoPic's affect a single picture.
     The other functions simply convert regular values such as integers
   and floats to the datatypes used by the functional programming.

************************************************************************-}


module R_Ptypes (Vec(..), Color(..), Pic(..), Poly(..), Movie(..), Behaviour(..), PictoPic(..), Process(..),
		 Vecfloat(..),
                 Msg(..), Chan(..),
                 Val (..),
                 ntov, vtov, nstov, vstov, pstov, bstov
                )   where


  --These are the basic data types for storing and manipulating the movies. 

type Vec = (Int,Int)
type Color = Int
type Pic = [Poly]
type Poly = (Color,[Vec])
type Movie = [Pic]
type Behaviour = [Pic -> Pic]
type PictoPic  = Pic -> Pic

type Process = [Msg] -> [Msg]
type Msg     = [(Chan,Val)]
type Chan    = [Char]

data Val     = N Int | V (Int,Int) | P Pic | B PictoPic

type Vecfloat = (Float,Float)



--Those convert from the various regular values to Val's.

ntov n   = N n

vtov:: Vec -> Val
vtov v   = V v

ptov:: Pic -> Val
ptov  p  = P p

nstov ns = [N n|n<-ns]

vstov:: [Vec] -> [Val]
vstov vs = [V v|v<-vs]

pstov:: [Pic] -> [Val]
pstov ps = [P p|p<-ps]

bstov:: [PictoPic] -> [Val]
bstov bs = [B b|b<-bs]
