-- I/O functions and definitions

module PreludeIO(stdin,stdout,stderr,stdecho,{-Request(..),Response(..),-}
                 IOError(..),Dialogue(..),IO(..),SystemState,IOResult,
                 SuccCont(..),StrCont(..),
                 StrListCont(..),BinCont(..),FailCont(..),
                 readFile, writeFile,  appendFile,  readBinFile,
                 writeBinFile,  appendBinFile,  deleteFile,  statusFile,
                 readChan,  appendChan,  readBinChan,  appendBinChan,
                 statusChan,  echo,  getArgs,  getProgName,  getEnv,  setEnv,
                 done, exit, abort, print, prints, interact,
		 thenIO,thenIO_,seqIO,returnIO, doneIO)
   where

import PreludeBltinIO
import PreludeBltinArray(strict1)

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

-- These datatypes are used by the monad.

type IO a = SystemState -> IOResult a

data SystemState = SystemState
data IOResult a = IOResult a

-- Operations in the monad

-- This definition is needed to allow proper tail recursion of the Lisp
-- code.  The use of strict1 forces f1 s (since getState is strict) before
-- the call to f2.  The optimizer removed getState and getRes from the
-- generated code.

{-# thenIO :: Inline #-}
thenIO f1 f2 s =
  let g = f1 s
      s' = getState g in
    strict1 s' (f2 (getRes g) s')

{-# thenIO_ :: Inline #-}
x `thenIO_` y = x `thenIO` \_ -> y
x `seqIO` y = x `thenIO` \_ -> y

-- The returnIO function is implemented directly as a primitive.
doneIO = returnIO ()


-- File and channel names:

stdin	    =  "stdin"
stdout      =  "stdout"
stderr      =  "stderr"
stdecho     =  "stdecho"


-- Requests and responses:

{-  Not used since streams are no longer supported:
data Request =	-- file system requests:
			  ReadFile      String         
			| WriteFile     String String
			| AppendFile    String String
			| ReadBinFile   String 
			| WriteBinFile  String Bin
			| AppendBinFile String Bin
			| DeleteFile    String
			| StatusFile    String
		-- channel system requests:
			| ReadChan	String 
			| AppendChan    String String
			| ReadBinChan   String 
			| AppendBinChan String Bin
			| StatusChan    String
		-- environment requests:
			| Echo          Bool
			| GetArgs
			| GetProgName
			| GetEnv        String
			| SetEnv        String String
		deriving Text

data Response =		  Success
			| Str String 
			| StrList [String]
			| Bn  Bin
			| Failure IOError
		deriving Text

-}

data IOError =		  WriteError   String
			| ReadError    String
			| SearchError  String
			| FormatError  String
			| OtherError   String
		deriving Text

-- Continuation-based I/O:

type Dialogue    =  IO ()
type SuccCont    =                Dialogue
type StrCont     =  String     -> Dialogue
type StrListCont =  [String]   -> Dialogue
type BinCont     =  Bin        -> Dialogue
type FailCont    =  IOError    -> Dialogue
 
done	      ::                                                Dialogue
readFile      :: String ->           FailCont -> StrCont     -> Dialogue
writeFile     :: String -> String -> FailCont -> SuccCont    -> Dialogue
appendFile    :: String -> String -> FailCont -> SuccCont    -> Dialogue
readBinFile   :: String ->           FailCont -> BinCont     -> Dialogue
writeBinFile  :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
appendBinFile :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
deleteFile    :: String ->           FailCont -> SuccCont    -> Dialogue
statusFile    :: String ->           FailCont -> StrCont     -> Dialogue
readChan      :: String ->           FailCont -> StrCont     -> Dialogue
appendChan    :: String -> String -> FailCont -> SuccCont    -> Dialogue
readBinChan   :: String ->           FailCont -> BinCont     -> Dialogue
appendBinChan :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
statusChan    :: String ->           FailCont -> StrCont     -> Dialogue
echo          :: Bool ->             FailCont -> SuccCont    -> Dialogue
getArgs	      ::		     FailCont -> StrListCont -> Dialogue
getProgName   ::		     FailCont -> StrCont     -> Dialogue
getEnv	      :: String ->	     FailCont -> StrCont     -> Dialogue
setEnv	      :: String -> String -> FailCont -> SuccCont    -> Dialogue

done = returnIO ()

readFile name fail succ =
    primReadStringFile name `thenIO` objDispatch fail succ

writeFile name contents fail succ =
    primWriteStringFile name contents `thenIO` succDispatch fail succ

appendFile name contents fail succ =
    primAppendStringFile name contents `thenIO` succDispatch fail succ

readBinFile name fail succ =
    primReadBinFile name `thenIO` objDispatch fail succ

writeBinFile name contents fail succ =
    primWriteBinFile name contents `thenIO` succDispatch fail succ

appendBinFile name contents fail succ =
    primAppendBinFile name contents `thenIO` succDispatch fail succ

deleteFile name fail succ =
    primDeleteFile name `thenIO` succDispatch fail succ

statusFile name fail succ =
    primStatusFile name `thenIO`
      (\status ->  case status of Succ s            -> succ s
                                  Fail msg          -> fail (SearchError msg))

readChan name fail succ =
 if name == stdin then
    primReadStdin `thenIO` succ
 else
    badChan fail name

appendChan name contents fail succ =
 if name == stdout then
    primWriteStdout contents `thenIO` succDispatch fail succ
 else
    badChan fail name

readBinChan name fail succ =
  if name == stdin then
    primReadBinStdin `thenIO` objDispatch fail succ
  else
    badChan fail name

appendBinChan name contents fail succ =
  if name == stdout then
    primWriteBinStdout contents `thenIO` succDispatch fail succ
  else
    badChan fail name

statusChan name fail succ =
  if name == stdin || name == stdout then
     succ "0 0"
  else
     fail (SearchError "Channel not defined")

echo bool fail succ =
  if bool then
     succ
  else
     fail (OtherError "Echo cannot be turned off")

getArgs fail succ =
  succ [""]

getProgName fail succ =
    succ "haskell"

getEnv name fail succ =
    primGetEnv name `thenIO` objDispatch fail succ

setEnv name val fail succ =
    fail (OtherError "setEnv not implemented")

objDispatch fail succ r = 
            case r of Succ s            -> succ s
                      Fail msg          -> fail (OtherError msg)

succDispatch fail succ r = 
            case r of Succ _            -> succ
                      Fail msg          -> fail (OtherError msg)

badChan f name = f (OtherError ("Improper IO Channel: " ++ name))

abort		:: FailCont
abort err	=  done

exit		:: FailCont
exit err	= appendChan stderr (msg ++ "\n") abort done
		  where msg = case err of ReadError s   -> s
		  			  WriteError s  -> s
		  			  SearchError s -> s
		      			  FormatError s -> s
		      			  OtherError s  -> s

print		:: (Text a) => a -> Dialogue
print x		=  appendChan stdout (show x) exit done
prints          :: (Text a) => a -> String -> Dialogue
prints x s	=  appendChan stdout (shows x s) exit done

interact	:: (String -> String) -> Dialogue
interact f	=  readChan stdin exit
			    (\x -> appendChan stdout (f x) exit done)

