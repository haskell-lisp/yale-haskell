module PreludeLocalIO where

import PreludeIOPrims
import PreludeIOMonad

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

data IOResponse a = Succ a | Fail String deriving Text

exec :: ([Response] -> [Request]) -> IO ()
{-
-- Sunderesh's original definition
exec p = case (p bottom) of
          [] -> unitIO ()
          (q:qs) -> processRequest q `bindIO` \r ->
                    exec (\rs -> tail (p (r:rs)))

bottom :: a
bottom = error "Should never be evaluated"
-}
-- modified from the existing compiler. no quadratic behavior
-- needs
-- pure :: IO a -> a
-- other alternatives:
-- 1. use reference cells
-- 2. implement exec in Lisp

exec p = os requests `bindIO` \x -> unitIO () where
    requests = p responses
    responses = pureIO (os requests)

os :: [Request] -> IO [Response]
os [] = unitIO []
os (q:qs) = processRequest q `bindIO` \r ->
            os qs `bindIO` \rs -> 
            unitIO (r:rs)

processRequest :: Request -> IO Response

-- This needs to be rewritten in terms of the continuation based defs

processRequest request =
  case request of

-- File system requests
   ReadFile name ->
      primReadStringFile name `bindIO` \a -> 
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   WriteFile name contents ->
      primWriteStringFile name contents `bindIO` \a -> 
        case a of 
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   AppendFile name contents ->
      primAppendStringFile name contents `bindIO` \a ->
        case a of
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   ReadBinFile name ->
      primReadBinFile name `bindIO` \a ->
        case a of
          Succ s -> unitIO (Bn s) 
          Fail e -> unitIO (Failure e)
   WriteBinFile name bin ->
      primWriteBinFile name bin `bindIO` \a ->
        case a of
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   AppendBinFile name bin ->
      primAppendBinFile name bin `bindIO` \a ->
        case a of
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   DeleteFile name ->
      primDeleteFile name `bindIO` \a ->
        case a of 
          MaybeNot -> Success
          Maybe e  -> unitIO (Failure e)
   StatusFile name ->
      primStatusFile name `bindIO` \a -> 
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)

-- Channel system requests
   ReadChan name ->
      primReadChan name `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   AppendChan name string ->
      primAppendChan name string `bindIO` \a ->
        case a of
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   ReadBinChan name ->
      primReadBinChan name `bindIO` \a ->
        case a of
          Succ s -> unitIO (Bn s)
          Fail e -> unitIO (Failure e)
   AppendBinChan name bin ->
      primAppendBinChan name bin `bindIO` \a ->
        case a of
          MaybeNot -> unitIO Success
          Maybe e  -> unitIO (Failure e)
   StatusChan name ->
      primStatusChan name `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)

-- Environment requests
   Echo status ->
      primEcho status `bindIO` \a -> 
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   GetArgs ->
      primGetArgs `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   GetProgName ->
      primProgArgs `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   GetEnv name ->
      primGetEnv name `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   SetEnv name string ->
      primGetEnv name string `bindIO` \a ->
        case a of
          Succ s -> unitIO (Str s)
          Fail e -> unitIO (Failure e)
   _ -> unitIO (Failure (OtherError "Unrecognized IO Feature"))

-- Monadic Style IO
-- Channel system requests

