module Seaside (main) where

import Animation
import Seafigs

seaside :: Movie
seaside = rOVERLAY [blue_sky,
		    apply (bPar [up,cw,movto (repeat botm)]) sun,
		    apply right clouds,
		    apply (bPar [right,bigger]) gull,
		    apply (bPar [right,right,bigger]) gull,
		    apply (bPar [up,up,right,bigger]) gull,
		    apply (bPar [up,right,right,right]) gull,
		    windm,
		    apply (mov (repeat botm)) palm,
		    man_and_vm
           	   ]
	where man_and_vm = rBESIDE2 [manfig, vm]
              manfig = apply left (apply (mov (i (700,0)))
 					         man)
	      windm = apply (mov (i (500,0))) windmill        	 


main = getEnv "DISPLAY" exit 
       (\ host -> displaym host 30 (map (flipy_Pic 500) seaside))
