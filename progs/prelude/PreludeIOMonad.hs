module IOMonad (State, IO(..)) where

import IOMonadPrims

{- I use data instead of type so that IO can be abstract. For efficiency,
   IO can be annotated as a strict constructor.
-}

type IO a = State -> (State, a)

data State = State

-- The rest of this file is unnecessary at the moment since
-- unitIO & bindIO are primitives and we're not using the rest of this

{- Implemented as a primitives: 
bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) (IO k) = IO (\s0 -> let (s1, a) = m s0 in k a s1) -}

unitIO :: a -> IO a
unitIO x = IO (\s -> (s, x))

-}

{-  Not currently used:
pureIO :: IO a -> a
pureIO (IO m) = let (s, x) = m State in x

-- execIO executes a program of type IO ().
execIO :: IO () -> State
execIO (IO m) = let (s, x) = m State in s

infixr  1 =:
infixr  1 ?

-- assignment
(=:)      :: a -> Var a -> IO ()
x =: v  = IO (\s -> (update v x s, ()))

-- reader
(?)       :: Var a -> (a -> IO b) -> IO b
v ? k   = IO (\s -> (s, readVar v s)) `bindIO` k

-- new
newvar    :: IO (Var a)
newvar = IO allocVar

instance Eq (Var a) where
   x == y = eqVar x y
-}










