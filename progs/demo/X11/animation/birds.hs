module Birds where

import Animation

bird :: Movie
--bird = osc [bird1,bird2]
bird = rOVERLAY
         [apply (bPar [right,right,right,right]) bm1,
          apply (bPar [up,right,right,right]) bm2]
         where bm1 = osc [bird1]
               bm2 = osc [bird2]

bird1 = [(black,b1)]
        where b1 = [(0,90),(20,100),(30,110),(40,110),(50,100),(110,120),
                    (130,100),(120,90),(80,90),(0,90),
                    (80,90),(90,70),(140,50),(120,90),(80,90),
                    (80,90),(70,70),(80,60),(90,70)]

bird2 = [(red,b2)]
        where b2 = [(0,60),(20,70),(30,80),(40,80),(50,70),(110,70),
                    (140,30),(110,35),(100,35),(70,50),(50,60),(0,60),
	            (70,50),(100,90),(150,100),(120,60),(110,35),
                    (70,50),(65,100),(85,115),(97,86)]

main = getEnv "DISPLAY" exit 
       (\ host -> displaym host 30 bird)


