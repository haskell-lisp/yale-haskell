-- Standard list functions

-- build really shouldn't be exported, but what the heck.
-- some of the helper functions in this file shouldn't be
-- exported either!

module PreludeList (PreludeList.., foldr, build) where

import PreludePrims(build, foldr)

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

infixl 9  !!
infix  5  \\
infixr 5  ++
infix  4 `elem`, `notElem`


-- These are primitives used by the deforestation stuff in the optimizer.
-- the optimizer will turn references to foldr and build into
-- inlineFoldr and inlineBuild, respectively, but doesn't want to
-- necessarily inline all references immediately.

inlineFoldr :: (a -> b -> b) -> b -> [a] -> b
inlineFoldr f z l =
  let foldr' []	 	= z
      foldr' (x:xs)	= f x (foldr' xs)
  in foldr' l
{-# inlineFoldr :: Inline #-}


inlineBuild :: ((a -> [a] -> [a]) -> [b] -> [c]) -> [c]
inlineBuild g           = g (:) []
{-# inlineBuild :: Inline #-}


-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.

head			:: [a] -> a
head (x:_)		=  x
head []			=  error "head{PreludeList}: head []"

last			:: [a] -> a
last [x]		=  x
last (_:xs)		=  last xs
last []			=  error "last{PreludeList}: last []"

tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "tail{PreludeList}: tail []"

init			:: [a] -> [a]
init [x]		=  []
init (x:xs)		=  x : init xs
init []			=  error "init{PreludeList}: init []"

-- null determines if a list is empty.
null			:: [a] -> Bool
null []			=  True
null (_:_)		=  False


-- list concatenation (right-associative)

(++)			:: [a] -> [a] -> [a]
xs ++ ys		= build (\ c n -> foldr c (foldr c n ys) xs)
{-# (++) :: Inline #-}


-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  Thus, (xs ++ ys) \\ xs == ys.
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\)			=  foldl del
			   where [] `del` _	    = []
				 (x:xs) `del` y
					| x == y    = xs
					| otherwise = x : xs `del` y

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.

genericLength		:: (Num a) => [b] -> a
genericLength l         = foldr (\ x n -> 1 + n) 0 l
--genericLength []	=  0
--genericLength (x:xs)    =  1 + genericLength xs
{-# genericLength :: Inline #-}


length			:: [a] -> Int
length l		= foldr (\ x n -> 1 + n) 0 l
--length []               = 0
--length (x:xs)           = 1 + length xs
{-# length :: Inline #-}

-- List index (subscript) operator, 0-origin
(!!)			:: (Integral a) => [b] -> a -> b
l !! i			=  nth l (fromIntegral i)
{-# (!!)  :: Inline #-}

nth                     :: [b] -> Int -> b
nth l m	= let f x g 0 = x
	      f x g i = g (i - 1)
	      fail _ = error "(!!){PreludeList}: index too large"
	  in foldr f fail l m
{-# nth  :: Inline #-}
--nth _ n  | n < 0	= error "(!!){PreludeList}: negative index"
--nth [] n		= error "(!!){PreludeList}: index too large"
--nth (x:xs) n 
--	| n == 0	= x
--	| otherwise     = nth xs (n - 1)
--{-# nth  :: Strictness("S,S") #-}

-- map f xs applies f to each element of xs; i.e., map f xs == [f x | x <- xs].
map			:: (a -> b) -> [a] -> [b]
map f xs		= build (\ c n -> foldr (\ a b -> c (f a) b) n xs)
--map f []		=  []
--map f (x:xs)		=  f x : map f xs
{-# map  :: Inline #-}


-- filter, applied to a predicate and a list, returns the list of those
-- elements that satisfy the predicate; i.e.,
-- filter p xs == [x | x <- xs, p x].
filter			:: (a -> Bool) -> [a] -> [a]
filter f xs		= build (\ c n ->
                                  foldr (\ a b -> if f a then c a b else b)
				  n xs)
--filter p		=  foldr (\x xs -> if p x then x:xs else xs) []
{-# filter  :: Inline #-}

 
-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i.e.,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p		=  foldr select ([],[])
			   where select x (ts,fs) | p x	      = (x:ts,fs)
						  | otherwise = (ts,x:fs)
{-# partition  :: Inline #-}


-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--	foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--	scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--	scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

foldl			:: (a -> b -> a) -> a -> [b] -> a
foldl f z xs            = foldr (\ b g a -> g (f a b)) id xs z
--foldl f z []		=  z
--foldl f z (x:xs)	=  foldl f (f z x) xs
{-# foldl  :: Inline #-}

foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)		=  foldl f x xs
foldl1 _ []		=  error "foldl1{PreludeList}: empty list"
{-# foldl1  :: Inline #-}

scanl			:: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs		=  q : (case xs of
				[]   -> []
				x:xs -> scanl f (f q x) xs)
{-# scanl  :: Inline #-}

scanl1			:: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)		=  scanl f x xs
scanl1 _ []		=  error "scanl1{PreludeList}: empty list"
{-# scanl1 :: Inline #-}


-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

--foldr			:: (a -> b -> b) -> b -> [a] -> b
--foldr f z []		=  z
--foldr f z (x:xs)	=  f x (foldr f z xs)


foldr1			:: (a -> a -> a) -> [a] -> a
foldr1 f [x]		=  x
foldr1 f (x:xs)		=  f x (foldr1 f xs)
foldr1 _ []		=  error "foldr1{PreludeList}: empty list"
{-# foldr1  :: Inline #-}


-- I'm not sure the build/foldr expansion wins.

scanr			:: (a -> b -> b) -> b -> [a] -> [b]
--scanr f q0 l = build (\ c n ->
--                        let g x qs@(q:_) = c (f x q) qs
--			in foldr g (c q0 n) l)
scanr f q0 []		=  [q0]
scanr f q0 (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr f q0 xs 
{-# scanr  :: Inline #-}

scanr1			:: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]		=  [x]
scanr1 f  (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr1 f xs 
scanr1 _ []		=  error "scanr1{PreludeList}: empty list"
{-# scanr1  :: Inline #-}


-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
iterate			:: (a -> a) -> a -> [a]
iterate f x	= build (\ c n ->
                          let iterate' x' = c x' (iterate' (f x'))
			  in iterate' x)
--iterate f x		=  x : iterate f (f x)
{-# iterate  :: Inline #-}


-- repeat x is an infinite list, with x the value of every element.
repeat			:: a -> [a]
repeat x		= build (\ c n -> let r = c x r in r)
--repeat x		=  xs where xs = x:xs
{-# repeat  :: Inline #-}

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle			:: [a] -> [a]
cycle xs		=  xs' where xs' = xs ++ xs'


-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).

take			:: (Integral a) => a -> [b] -> [b]
take n l		= takeInt (fromIntegral n) l
{-# take  :: Inline #-}

takeInt                 :: Int -> [b] -> [b]
takeInt m l = 
  build (\ c n ->
           let f x g i | i <= 0		= n
	               | otherwise      = c x (g (i - 1))
           in foldr f (\ _ -> n) l m)
--takeInt  0     _	=  []
--takeInt  _     []	=  []
--takeInt  n l | n > 0    = primTake n l
{-# takeInt  :: Inline #-}



-- Writing drop and friends in terms of build/foldr seems to lose
-- way big since they cause an extra traversal of the list tail
-- (except when the calls are being deforested).

drop			:: (Integral a) => a -> [b] -> [b]
drop n l		= dropInt (fromIntegral n) l
{-# drop  :: Inline #-}
{-# drop  :: Strictness("S,S") #-}


dropInt                 :: Int -> [b] -> [b]
dropInt  0     xs	=  xs
dropInt  _     []	=  []
dropInt (n+1) (_:xs)	=  dropInt n xs
{-# dropInt  :: Inline #-}

splitAt			:: (Integral a) => a -> [b] -> ([b],[b])
splitAt n l		= splitAtInt (fromIntegral n) l
{-# splitAt  :: Inline #-}

splitAtInt		:: Int -> [b] -> ([b],[b])
splitAtInt  0     xs	=  ([],xs)
splitAtInt  _     []	=  ([],[])
splitAtInt (n+1) (x:xs)	=  (x:xs',xs'') where (xs',xs'') = splitAtInt n xs
{-# splitAtInt  :: Inline #-}

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

takeWhile		:: (a -> Bool) -> [a] -> [a]
takeWhile p l = build (\ c n -> foldr (\ a b -> if p a then c a b else n) n l)
--takeWhile p []		=  []
--takeWhile p (x:xs) 
--            | p x       =  x : takeWhile p xs
--            | otherwise =  []
{-# takeWhile  :: Inline #-}


dropWhile		:: (a -> Bool) -> [a] -> [a]
dropWhile p []		=  []
dropWhile p xs@(x:xs')
	    | p x       =  dropWhile p xs'
	    | otherwise =  xs
{-# dropWhile  :: Inline #-}

span, break		:: (a -> Bool) -> [a] -> ([a],[a])
span p []		=  ([],[])
span p xs@(x:xs')
	   | p x	=  let (ys,zs) = span p xs' in (x:ys,zs)
	   | otherwise	=  ([],xs)
break p			=  span (not . p)

{-# span  :: Inline #-}
{-# break  :: Inline #-}


-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.

lines			:: String -> [String]
lines ""		=  []
lines s			=  let (l, s') = break (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> lines s''

words			:: String -> [String]
words s			=  case dropWhile isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = break isSpace s'

unlines			:: [String] -> String
unlines			=  concat . map (++ "\n")
{-# unlines  :: Inline #-}


unwords			:: [String] -> String
unwords []		=  ""
unwords ws		=  foldr1 (\w s -> w ++ ' ':s) ws

-- nub (meaning "essence") removes duplicate elements from its list argument.
nub			:: (Eq a) => [a] -> [a]
nub l = build (\ c n ->
                 let f x g [] = c x (g [x])
		     f x g xs = if elem x xs
		                   then (g xs)
				   else c x (g (x:xs))
                 in foldr f (\ _ -> n) l [])
{-# nub  :: Inline #-}
--nub []			=  []
--nub (x:xs)		=  x : nub (filter (/= x) xs)

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
reverse			:: [a] -> [a]
reverse l = build (\ c n ->
                     let f x g tail = g (c x tail)
		     in foldr f id l n)
{-# reverse  :: Inline #-}
--reverse x               =  reverse1 x [] where
--  reverse1 [] a     = a
--  reverse1 (x:xs) a = reverse1 xs (x:a)

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or			:: [Bool] -> Bool
and			=  foldr (&&) True
or			=  foldr (||) False
{-# and :: Inline #-}
{-# or  :: Inline #-}

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all		:: (a -> Bool) -> [a] -> Bool
any p			=  or . map p
all p			=  and . map p
{-# any :: Inline #-}
{-# all :: Inline #-}

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
elem, notElem		:: (Eq a) => a -> [a] -> Bool

elem x ys = foldr (\ y t -> (x == y) || t) False ys
--x `elem` []		=  False
--x `elem` (y:ys)         =  x == y || x `elem` ys
{-# elem :: Inline #-}
notElem	x y		=  not (x `elem` y)

-- sum and product compute the sum or product of a finite list of numbers.
sum, product		:: (Num a) => [a] -> a
sum			=  foldl (+) 0	
product			=  foldl (*) 1
{-# sum :: Inline #-}
{-# product :: Inline #-}

-- sums and products give a list of running sums or products from
-- a list of numbers.  For example,  sums [1,2,3] == [0,1,3,6].
sums, products		:: (Num a) => [a] -> [a]
sums			=  scanl (+) 0
products		=  scanl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
maximum, minimum	:: (Ord a) => [a] -> a
maximum			=  foldl1 max
minimum			=  foldl1 min
{-# maximum :: Inline #-}
{-# minimum :: Inline #-}

-- concat, applied to a list of lists, returns their flattened concatenation.
concat			:: [[a]] -> [a]
concat xs	= build (\ c n -> foldr (\ x y -> foldr c y x) n xs)
--concat []               =  []
--concat (l:ls)           =  l ++ concat ls
{-# concat :: Inline #-}


-- transpose, applied to a list of lists, returns that list with the
-- "rows" and "columns" interchanged.  The input need not be rectangular
-- (a list of equal-length lists) to be completely transposable, but can
-- be "triangular":  Each successive component list must be not longer
-- than the previous one; any elements outside of the "triangular"
-- transposable region are lost.  The input can be infinite in either
-- dimension or both.
transpose		:: [[a]] -> [[a]]
transpose		=  foldr 
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     []
{-# transpose :: Inline #-}

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.
-- zip3 takes three lists and returns a list of triples, etc.  Versions
-- of zip producing up to septuplets are defined here.

zip			:: [a] -> [b] -> [(a,b)]
zip			=  zipWith (\a b -> (a,b))
{-# zip :: Inline #-}

zip3			:: [a] -> [b] -> [c] -> [(a,b,c)]
zip3			=  zipWith3 (\a b c -> (a,b,c))
{-# zip3 :: Inline #-}

zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4			=  zipWith4 (\a b c d -> (a,b,c,d))
{-# zip4 :: Inline #-}

zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5			=  zipWith5 (\a b c d e -> (a,b,c,d,e))
{-# zip5 :: Inline #-}

zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f]
			   -> [(a,b,c,d,e,f)]
zip6			=  zipWith6 (\a b c d e f -> (a,b,c,d,e,f))
{-# zip6 :: Inline #-}

zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
			   -> [(a,b,c,d,e,f,g)]
zip7			=  zipWith7 (\a b c d e f g -> (a,b,c,d,e,f,g))
{-# zip7 :: Inline #-}

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.

zipWith			:: (a->b->c) -> [a]->[b]->[c]
zipWith z as bs =
  build (\ c' n' ->
           let f' a g' (b:bs) = c' (z a b) (g' bs)
	       f' a g' _ = n'
           in foldr f' (\ _ -> n') as bs)
--zipWith z (a:as) (b:bs)	=  z a b : zipWith z as bs
--zipWith _ _ _		=  []
{-# zipWith :: Inline #-}

zipWith3		:: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z as bs cs =
  build (\ c' n' ->
          let f' a g' (b:bs) (c:cs) = c' (z a b c) (g' bs cs)
              f' a g' _ _ = n'
          in foldr f' (\ _ _ -> n') as bs cs)
{-# zipWith3 :: Inline #-}
--zipWith3 z (a:as) (b:bs) (c:cs)
--			=  z a b c : zipWith3 z as bs cs
--zipWith3 _ _ _ _	=  []

zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z as bs cs ds =
  build (\ c' n' ->
          let f' a g' (b:bs) (c:cs) (d:ds) = c' (z a b c d) (g' bs cs ds)
              f' a g' _ _ _ = n'
          in foldr f' (\ _ _ _ -> n') as bs cs ds)
{-# zipWith4 :: Inline #-}
--zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
--			=  z a b c d : zipWith4 z as bs cs ds
--zipWith4 _ _ _ _ _	=  []

zipWith5		:: (a->b->c->d->e->f)
			   -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z as bs cs ds es=
  build (\ c' n' ->
          let f' a g' (b:bs) (c:cs) (d:ds) (e:es) =
	        c' (z a b c d e) (g' bs cs ds es)
              f' a g' _ _ _ _ = n'
          in foldr f' (\ _ _ _ _ -> n') as bs cs ds es)
{-# zipWith5 :: Inline #-}
--zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
--			=  z a b c d e : zipWith5 z as bs cs ds es
--zipWith5 _ _ _ _ _ _	=  []

zipWith6		:: (a->b->c->d->e->f->g)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z as bs cs ds es fs =
  build (\ c' n' ->
          let f' a g' (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
	        c' (z a b c d e f) (g' bs cs ds es fs)
              f' a g' _ _ _ _ _ = n'
          in foldr f' (\ _ _ _ _ _ -> n') as bs cs ds es fs)
{-# zipWith6 :: Inline #-}
--zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
--			=  z a b c d e f : zipWith6 z as bs cs ds es fs
--zipWith6 _ _ _ _ _ _ _	=  []

zipWith7		:: (a->b->c->d->e->f->g->h)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z as bs cs ds es fs gs =
  build (\ c' n' ->
          let f' a g' (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) =
	        c' (z a b c d e f g) (g' bs cs ds es fs gs)
              f' a g' _ _ _ _ _ _ = n'
          in foldr f' (\ _ _ _ _ _ _ -> n') as bs cs ds es fs gs)
{-# zipWith7 :: Inline #-}
--zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
--		   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
--zipWith7 _ _ _ _ _ _ _ _ =  []


-- unzip transforms a list of pairs into a pair of lists.  As with zip,
-- a family of such functions up to septuplets is provided.

unzip			:: [(a,b)] -> ([a],[b])
unzip			=  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
{-# unzip :: Inline #-}


unzip3			:: [(a,b,c)] -> ([a],[b],[c])
unzip3			=  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				 ([],[],[])
{-# unzip3 :: Inline #-}

unzip4			:: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4			=  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
					(a:as,b:bs,c:cs,d:ds))
				 ([],[],[],[])
{-# unzip4 :: Inline #-}

unzip5			:: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5			=  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
					(a:as,b:bs,c:cs,d:ds,e:es))
				 ([],[],[],[],[])
{-# unzip5 :: Inline #-}

unzip6			:: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6			=  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs))
				 ([],[],[],[],[],[])
{-# unzip6 :: Inline #-}

unzip7			:: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7			=  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
				 ([],[],[],[],[],[],[])
{-# unzip7 :: Inline #-}

