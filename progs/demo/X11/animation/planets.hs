module Planets (main) where

import Animation

planets:: Float -> Float -> Int -> Int -> Int -> Int -> Movie
planets i1 i2 r1 r2 c1 c2
    = rOVERLAY
       [ apply f1 earth,
         apply (bpar f1 f2) moon
       ]
        where f1 = circ_mov (fromIntegral r1) i1
              f2 = circ_mov (fromIntegral r2) i2
              earth = osc [mov_Pic (vplus center (r1,0)) (box c1 30 30)]
              moon = osc [mov_Pic (vplus center (r1+r2,0)) (box c2 15 15)]

gen a b c d = c :(gen a b (c+b) d)


planet_scene:: Movie
planet_scene = rOVERLAY
                 [apply (bpar (set_color (i yellow)) (movto (i center))) orb,
                  planets (pi/40.0) (pi/10.0) 450 80 darkblue lightblue,
                  planets (pi/20.0) (pi/8.0) 300 50 brown black,
                  planets (pi/10.0) (pi/4.0) 150 40 green red
                 ]

orb = osc [circ red 50 10]

main = getEnv "DISPLAY" exit 
       (\ host -> displaym host 60 planet_scene)
