{- This is the n Queens problem. -}

module Main where

queens :: Int -> [[Int]]
queens size  = queens' size size

queens' :: Int -> Int -> [[Int]]
queens' 0     _    = [[]]
queens' (n+1) size = [q:qs | qs <- queens' n size, q <- [1..size],
			     not (threatens q qs)]

threatens :: Int -> [Int] -> Bool
threatens q qs = q `elem` qs || q `elem` (diagonals 1 qs)

diagonals :: Int -> [Int] -> [Int]
diagonals _  []    = []
diagonals n (q:qs) = (q+n) : (q-n) : diagonals (n+1) qs

main = appendChan stdout "Enter board size: " abort $
       readChan stdin abort $ \input -> 
       let line1 : ~(line2 : _) = lines input
	   size = read line1
           solns = read line2
       in if size == 0 then done else  -- This causes the size to actually read
         appendChan stdout "Number of solutions: " abort $
         appendChan stdout (concat (map (\x -> showBoard size x)
                                        (take solns (queens size))))
  	 abort done

showBoard :: Int -> [Int] -> String

showBoard size pos =
  concat (map showRow pos) ++ "\n"
    where
      showRow n = concat [if i == n then "Q " else ". " | i <- [1..size]]
                  ++ "\n"
                


