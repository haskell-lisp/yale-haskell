module Seafigs (sky,blue_sky,clouds,clouds2,gull,man,sun,vm,windmill,palm) where

import Animation

blue_sky:: Movie
blue_sky = osc [box lightblue 1000 1000]

sky:: Color -> Movie
sky c = osc [box c 1000 1000]

clouds2:: Movie
clouds2 = apply (mov (i (cld_wid,0))) (rBESIDE[cld,cld])
         where cld_wid = -(wid_Pic (cld!!0))
               cld= apply (bPar [right,mov (repeat (250,-50))]) cldm1
               cldm1=osc[cloud1]
 
clouds:: Movie
clouds
  = rOVERLAY
      [apply (bPar [right,mov (repeat (250,-50))]) cloudm1,
       apply (bPar [right,mov (repeat (0,-50))]) cloudm2,
       apply (bPar [right,mov (repeat (250,-75))]) cloudm2,
       apply (bPar [right,flipb,smaller,mov(repeat (200,-100))]) cloudm2,
       apply (bPar [right,flipb,smaller,mov(repeat (300,-125))]) cloudm1,
       apply (bPar [right,right,mov (repeat (-50,50))]) cloudm1]
       where cloudm1 = osc [cloud1]
             cloudm2 = osc [cloud2]


cloud1 = [(white,ply)]
         where ply = [(142,301),(169,309),(180,315),(192,312),
                      (196,308),(202,302),(216,300),(224,308),
                      (238,312),(258,311),(274,301),(278,283),
                      (265,279),(246,279),(230,281),(197,286),
                      (185,288),(167,287),(148,287),(136,292),
                      (136,292),(142,301)]


cloud2 = [(white,ply)]
         where ply = [(51,262), (56,266),
                      (66,265), (90,264), (92,266), (98,270),
                      (111,268),(137,268),(155,266),(174,266),
                      (183,262),(183,253),(162,251),(136,254),
                      (132,250),(126,248),(115,252),(109,253),
                      (98,252), (90,253), (88,254), (67,254),
                      (56,252), (49,254), (47,259), (51,262)]

gull :: Movie
gull = osc [gull1,gull2]

gull1 = [(black,[(2,4),(6,4),(9,2),(10,0),(11,2),
               (16,4),(20,4)])]

gull2 = [(black,[(0,0),(2,2),(6,3),(9,2),(12,3),
               (16,2),(18,0)])]

man :: Movie
man = osc [man1,man2,man3]


man1 = [(black,[(0,0),(10,0),(20,40),(30,60),(40,20),
                (60,0),(50,0)]),
        (black,[(0,40),(20,60),(30,80),(50,70),(60,60)]),
        (black,[(30,60),(30,100)]),
        (black,[(30,100),(25,100),(20,105),(23,112),
                (20,115),(30,120),(35,120),(40,115),
                (40,110),(35,105),(30,100)])
                 ]

man2 = [(black,[(20,0),(30,0),(20,40),(30,60),(45,30),
                (60,20),(50,0)]),
        (black,[(0,60),(20,60),(20,80),(40,80),(50,60)]),
        (black,[(30,60),(20,100)]),
        (black,[(20,100),(15,100),(10,105),(13,112),
                (10,115),(20,120),(30,120),(30,115),
                (30,110),(25,105),(20,100)])
                 ]

man3 = [(black,[(0,15),(5,10),(15,45),(30,60),(35,25),
            (44,10),(35,0)]),
        (black,[(10,40),(22,60),(20,80),(40,75),(45,44)]),
        (black,[(30,60),(20,100)]),
        (black,[(20,100),(19,100),(14,105),(17,112),
                (14,115),(24,120),(34,120),(34,115),
                (34,110),(29,105),(200,100)])
                 ]

sun :: Movie
sun = osc [sun']
      where
      sun' = reduce overlay_Pic [sun1,
                                 twist_Pic (pi/24.0) sun1,
                                 twist_Pic (pi/12.0) sun1]

sun1 = [(yellow,[(43,16),(18,27),(9,51),(20,71),(42,81),
                 (66,73),(76,47),(69,25),(43,15),(43,16)])]

vm :: Movie
vm =  osc[vm1,vm2]

vm1 = beside_Pic (box brown 10 15)
                 (above_Pic light1 (box brown 40 80))
      where light1 = box yellow 10 10

vm2 = beside_Pic (box brown 10 15)
                 (reduce above_Pic [light,light2,box brown 40 80])
      where light2 = over_Pic (box red 10 10) (box white 5 5)
            light  = [ (red,[(5,5), (10,2), (0,30),(5,5)]),
                       (red,[(20,2),(25,5),(30,30),(20,2)]),
                       (red,[(15,15),(20,15),(15,50),(10,25)])]

windmill :: Movie
windmill
   = apply
       (bpar (mov (repeat (unit*3,0))) (scale_rel (0,0) (repeat 3)))
       (overlay body (apply (movto (repeat (100,400))) prop))

blade = osc [tri red (0,0) (100,0) (50,300)]
prop  = apply cw fan

fan  = rOVERLAY [fan1,fan2,fan3,fan4]
fan1 = blade
fan2 = apply (rot (osc[(50,300)]) (osc[pi/2.0])) fan1
fan3 = apply (rot (osc[(50,300)]) (osc[pi/2.0])) fan2
fan4 = apply (rot (osc[(50,300)]) (osc[pi/2.0])) fan3

body = osc [ [(brown,[(0,0),(200,0),(170,300),
                     (100,400),(30,300),(0,0)]) ] ]


palm :: Movie
palm
  = osc palms
    where palms = inbetween 3 palm1 (flipx_Pic 100 palm1)
          palm1 = reduce overlay_Pic [trunk,frond1,frond2,frond3,frond4]
              where frond1 = [ (green,[(50,60),(60,70),(80,60)]),
                               (green,[(50,70),(60,80),(80,70)]),
                               (green,[(50,80),(55,90),(70,80)]),
                               (green,[(60,70),(55,90),(50,100)]) ]

                    frond2 = flipx_Pic 50 frond1

                    frond3 = [ (green,[(10,70),(5,80)]),
                               (green,[(10,80),(10,90)]),
                               (green,[(20,90),(20,100)]),
                               (green,[(30,95),(40,104)]),
                               (green,[(5,80),(20,100),(40,104),
                                       (50,100)])]

                    frond4 = [(green,[(0,100),(5,110)]),
                              (green,[(15,105),(15,115)]),
                              (green,[(25,105),(30,115)]),
                              (green,[(35,105),(40,115)]),
                              (green,[(5,110),(30,115),(50,110),
                                      (50,100)])]

                    trunk  = [(brown,[(100,0),(95,40),(80,80),
                                      (70,90),(60,97),(50,100)])]
