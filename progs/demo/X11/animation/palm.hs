module Palm (main) where

import Animation
import SeaFigs

main = getEnv "DISPLAY" exit 
       (\ host -> displaym host 30 trans)

trans :: Movie
trans = manright++change++gull2

manright::Movie
manright =  mirrorx (take 10 (apply left man))

gull2::Movie
gull2 = apply (bPar [right,up,huge,huge,huge,(mov (i (275,0)))])  gull

change::Movie
change = inbetween 5  manf1 gull1
              where gull1 = head gull2
	            manf1 = last manright



mirrorx :: Movie -> Movie
mirrorx m = map (flipx_Pic x) m 
              where (x,_)=orig_Movie m
               

orig_Movie :: Movie -> Vec
orig_Movie m = ((x2-x1) `div` 2,(y2-y1) `div` 2)
                  where x2 = reduce max (map maxx m)
                        x1 = reduce min (map minx m)
			y2 = reduce max (map maxy m)
			y1 = reduce min (map miny m)

maxx :: Pic -> Int
maxx p = reduce max [x | (c,q) <- p, (x,y) <- q]

minx :: Pic -> Int
minx p = reduce min [x | (c,q) <- p, (x,y) <- q]

maxy :: Pic -> Int
maxy p = reduce max [y | (c,q) <- p, (x,y) <- q]

miny :: Pic -> Int
miny p = reduce min [y | (c,q) <- p, (x,y) <- q]
