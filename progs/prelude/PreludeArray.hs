module  PreludeArray ( Array, Assoc((:=)), array, listArray, (!), bounds,
		     indices, elems, assocs, accumArray, (//), accum, amap,
		     ixmap
		   ) where

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

-- This module uses some simple techniques with updatable vectors to
-- avoid vector copying in loops where single threading is obvious.
-- This is rather fragile and depends on the way the compiler handles
-- strictness.

import PreludeBltinArray

infixl 9  !
infixl 9  //
infix  1  :=

data  Assoc a b =  a := b  deriving (Eq, Ord, Ix, Text, Binary)
data  (Ix a)    => Array a b = MkArray (a,a) {-#STRICT#-}
                                       (Vector (Box b)) {-#STRICT#-}
				       deriving ()

array		:: (Ix a) => (a,a) -> [Assoc a b] -> Array a b
listArray	:: (Ix a) => (a,a) -> [b] -> Array a b
(!)		:: (Ix a) => Array a b -> a -> b
bounds		:: (Ix a) => Array a b -> (a,a)
indices		:: (Ix a) => Array a b -> [a]
elems		:: (Ix a) => Array a b -> [b]
assocs		:: (Ix a) => Array a b -> [Assoc a b]
accumArray	:: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [Assoc a c]
			     -> Array a b
(//)		:: (Ix a) => Array a b -> [Assoc a b] -> Array a b
accum		:: (Ix a) => (b -> c -> b) -> Array a b -> [Assoc a c]
			     -> Array a b
amap		:: (Ix a) => (b -> c) -> Array a b -> Array a c
ixmap		:: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
			     -> Array a c

-- Arrays are a datatype containing a bounds pair and a vector of values.
-- Uninitialized array elements contain an error value.

-- Primitive vectors now contain only unboxed values.  This permits us to
-- treat array indexing as an atomic operation without forcing the element
-- being accessed.  The boxing and unboxing of array elements happens
-- explicitly using these operations:

data Box a = MkBox a
unBox (MkBox x) = x
{-# unBox :: Inline #-}


-- Array construction and update using index/value associations share
-- the same helper function.

array b@(bmin, bmax) ivs =
  let size = (index b bmax) + 1
      v = primMakeVector size uninitializedArrayError
  in (MkArray b (updateArrayIvs b v ivs))
{-# array :: Inline #-}

a@(MkArray b v) // ivs =
  let v' = primCopyVector v
  in (MkArray b (updateArrayIvs b v' ivs))
{-# (//) :: Inline #-}

updateArrayIvs b v ivs = 
  let g (i := x) next =  strict1 (primVectorUpdate v (index b i) (MkBox x))
                                 next
  in foldr g v ivs
{-# updateArrayIvs :: Inline #-}

uninitializedArrayError = 
  MkBox (error "(!){PreludeArray}: uninitialized array element.")


-- when mapping a list onto an array, be smart and don't do full index 
-- computation

listArray b@(bmin, bmax) vs =
  let size = (index b bmax) + 1
      v = primMakeVector size uninitializedArrayError
  in (MkArray b (updateArrayVs size v vs))
{-# listArray :: Inline #-}

updateArrayVs size v vs =
  let g x next j = if (j == size)
                     then v
		     else strict1 (primVectorUpdate v j (MkBox x))
		                  (next (j + 1))
  in foldr g (\ _ -> v) vs 0
{-# updateArrayVs :: Inline #-}


-- Array access

a@(MkArray b v) ! i = unBox (primVectorSel v (index b i))
{-# (!) :: Inline #-}

bounds (MkArray b _)  = b

indices		      = range . bounds


-- Again, when mapping array elements into a list, be smart and don't do 
-- the full index computation for every element.

elems a@(MkArray b@(bmin, bmax) v) =
  build (\ c n -> 
          let size = (index b bmax) + 1
	      g j  = if (j == size)
	                then n
			else c (unBox (primVectorSel v j)) (g (j + 1))
          -- This strict1 is so size doesn't get inlined and recomputed
	  -- at every iteration.  It should also force the array argument
	  -- to be strict.
          in strict1 size (g 0))
{-# elems :: Inline #-}

assocs a@(MkArray b@(bmin, bmax) v) =
  build (\ c n ->
          let g i next j = let y = unBox (primVectorSel v j)
                           in c (i := y) (next (j + 1))
	  in foldr g (\ _ -> n) (range b) 0)
{-# assocs :: Inline #-}


-- accum and accumArray share the same helper function.  The difference is
-- that accum makes a copy of an existing array and accumArray creates
-- a new one with all elements initialized to the given value.

accum f a@(MkArray b v) ivs =
  let v' = primCopyVector v
  in (MkArray b (accumArrayIvs f b v' ivs))
{-# accum :: Inline #-}

accumArray f z b@(bmin, bmax) ivs =
  let size = (index b bmax) + 1
      v = primMakeVector size (MkBox z)
  in (MkArray b (accumArrayIvs f b v ivs))
{-# accumArray :: Inline #-}


-- This is a bit tricky.  We need to force the access to the array element
-- before the update, but not force the thunk that is the value of the
-- array element unless f is strict.

accumArrayIvs f b v ivs =
  let g (i := x) next = 
        let j = index b i
	    y = primVectorSel v j
	in strict1
	     y
	     (strict1 (primVectorUpdate v j (MkBox (f (unBox y) x)))
	              next)
  in foldr g v ivs
{-# accumArrayIvs :: Inline #-}


-- again, be smart and bypass full array indexing on array mapping

amap f a@(MkArray b@(bmin, bmax) v) =
  let size = (index b bmax) + 1
      v' = primMakeVector size uninitializedArrayError
      g j = if (j == size)
              then v'
	      else let y = primVectorSel v j
	           in strict1 (primVectorUpdate v' j (MkBox (f (unBox y))))
	                      (g (j + 1))
  in (MkArray b (g 0))
{-# amap :: Inline #-}


-- can't bypass the index computation here since f needs it as an argument

ixmap b f a           = array b [i := a ! f i | i <- range b]
{-# ixmap :: Inline #-}


-- random other stuff

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'  	    	=  assocs a <=  assocs a'

instance  (Ix a, Text a, Text b) => Text (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ]
		  ++
		  [(listArray b xs, u) | ("listArray",s) <- lex r,
					 (b,t)           <- reads s,
					 (xs,u)          <- reads t ])
