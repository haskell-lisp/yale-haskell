-- maybe.hs -- "maybe" type
--
-- author :  Sandra Loosemore
-- date   :  22 June 1993
--

module Maybe where

data Maybe a = Some a | Null

{-# ImportLispType (Maybe(Some("identity", "identity", "identity"),
                          Null("not", "'#f")))  #-}
