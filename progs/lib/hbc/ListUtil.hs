module ListUtil(assoc, concatMap, unfoldr, mapAccuml, union, intersection, chopList, assocDef, lookup, Maybe..) where
import Maybe

-- Lookup an item in an association list.  Apply a function to it if it is found, otherwise return a default value.
assoc :: (Eq c) => (a -> b) -> b -> [(c, a)] -> c -> b
assoc f d [] x                       = d
assoc f d ((x',y):xys) x | x' == x   = f y
                         | otherwise = assoc f d xys x

-- Map and concatename results.
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []	   = []
concatMap f (x:xs) =
	case f x of
	[] -> concatMap f xs
	ys -> ys ++ concatMap f xs

-- Repeatedly extract (and transform) values until a predicate hold.  Return the list of values.
unfoldr :: (a -> (b, a)) -> (a -> Bool) -> a -> [b]
unfoldr f p x | p x       = []
	      | otherwise = y:unfoldr f p x'
			      where (y, x') = f x

-- Map, but plumb a state through the map operation.
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f s []     = (s, [])
mapAccuml f s (x:xs) = (s'', y:ys)
		       where (s',  y)  = f s x
			     (s'', ys) = mapAccuml f s' xs

-- Union of sets as lists.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ (ys \\ xs)

-- Intersection of sets as lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x<-xs, x `elem` ys]

--- Functions derived from those above

chopList :: ([a] -> (b, [a])) -> [a] -> [b]
chopList f l = unfoldr f null l

assocDef :: (Eq a) => [(a, b)] -> b -> a -> b
assocDef l d x = assoc id d l x

lookup :: (Eq a) => [(a, b)] -> a -> Maybe b
lookup l x = assoc Just Nothing l x
