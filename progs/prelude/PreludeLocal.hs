module PreludeLocal where

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

infixr 5 :

data Int = MkInt
data Integer = MkInteger
data Float = MkFloat
data Double   = MkDouble
data Char = MkChar
data Bin = MkBin
data List a = a : (List a) | Nil
data Arrow a b = MkArrow a b

data Triv = MkTriv
