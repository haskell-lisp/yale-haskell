module Hash where
--
-- Hash a value.  Hashing produces an Int of
-- unspecified range.
--

class Hashable a where
    hash :: a -> Int

instance Hashable Char where
    hash x = ord x

instance Hashable Int where
    hash x = x

instance Hashable Integer where
    hash x = fromInteger x

instance Hashable Float where
    hash x = truncate x

instance Hashable Double where
    hash x = truncate x

instance Hashable Bin where
    hash x = 0

{-instance Hashable File where
    hash x = 0 -}

instance Hashable () where
    hash x = 0

instance Hashable (a -> b) where
    hash x = 0

instance Hashable a => Hashable [a] where
    hash x = sum (map hash x)

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (a,b) = hash a + 3 * hash b

instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hash (a,b,c) = hash a + 3 * hash b + 5 * hash c

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d) where
    hash (a,b,c,d) = hash a + 3 * hash b + 5 * hash c + 7 * hash d

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (a,b,c,d,e) where
    hash (a,b,c,d,e) = hash a + hash b + hash c + hash d + hash e

instance Hashable Bool where
    hash False = 0
    hash True = 1

instance (Integral a, Hashable a) => Hashable (Ratio a) where
    hash x = hash (denominator x) + hash (numerator x)

instance (RealFloat a, Hashable a) => Hashable (Complex a) where
    hash (x :+ y) = hash x + hash y

instance (Hashable a, Hashable b) => Hashable (Assoc a b) where
    hash (x := y) = hash x + hash y

instance (Ix a) => Hashable (Array a b) where
    hash x = 0 -- !!!

instance Hashable Request where
    hash x = 0 -- !!

instance Hashable Response where
    hash x = 0 -- !!

instance Hashable IOError where
    hash x = 0 -- !!

hashToMax maxhash x =
    let h = abs (hash x)
    in  if h < 0 then 0 else h `rem` maxhash
