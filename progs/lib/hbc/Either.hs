module Either(Either(..)) where
data Either a b = Left a | Right b deriving (Eq, Ord, Text, Binary)
