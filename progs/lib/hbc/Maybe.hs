module Maybe(Maybe(..), thenM) where
-- Maybe together with Just and thenM forms a monad, but is more
-- by accident than by design.
data Maybe a = Nothing | Just a	deriving (Eq, Ord, Text, Binary)
Nothing `thenM` _ = Nothing
Just a  `thenM` f = f a
