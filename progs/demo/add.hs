-- this is an interactive program to read in two numbers and print their sum.

module Main where

main = readChan stdin abort $ \userInput -> 
       let inputLines = lines userInput in
        readInt "Enter first number: " inputLines $ \num1 inputLines1 ->
        readInt "Enter second number: " inputLines1 $ \ num2 _ ->
        appendChan stdout ("Their sum is: " ++ show (num1 + num2)) abort done

readInt :: String -> [String] -> (Integer -> [String] -> Dialogue) -> Dialogue

readInt prompt inputLines succ =
  appendChan stdout prompt abort $
  case inputLines of
     (l1 : rest) -> case (reads l1) of
                       [(x,"")] -> succ x rest
                       _        -> appendChan stdout
	                              "Error - retype the number\n" abort $
	                           readInt prompt rest succ
     _          -> appendChan stdout "Early EOF" abort done
