{-

Ki-Wing Ho and Eric Fox
Computer Science 429b
Professor Hudak
Final Project:  LOGO Interpreter

-}



-------------------------------------------------------------------------------
module REPLoop where

{-

REPLoop has two main parts: the first part (function logo) sets up the
graphics window, prints a welcome message, initializes the variable
and procedure environments and the turtle, accepts and lines's the
user input, runs the read-eval-print loop (part two), and then closes
the graphics window and exists; the second part (function repLoop)
lexes and parses each command, prints an error message if there was a
syntax error and evaluates (or tries to) if there wasn't, and then
either prints the value or an error message or exits if the value
returnd by the evaluator is "GoodBye".

-}

import Lexer
import Parser
import Evaluator
import Xlib

demo = main

main = getEnv "DISPLAY" exit $ \ host ->
       xHandleError ( \ (XError msg) -> appendChan stdout msg exit done) $
       logo host

logo :: String -> IO ()

logo host =
  xOpenDisplay host `thenIO` \ display ->

  let (screen:_) = xDisplayRoots display
      fg_color = xScreenWhitePixel screen
      bg_color = xScreenBlackPixel screen
      root = xScreenRoot screen
  in
  xCreateWindow root
                (XRect 100 100 500 500)
                [XWinBackground bg_color,
                 XWinBackingStore XAlwaysBackStore] 
  `thenIO` \ graphWindow ->
  xSetWmName graphWindow "Logo" `thenIO` \ () ->
  xSetWmIconName graphWindow "Logo" `thenIO` \ () ->
  xMapWindow graphWindow `thenIO` \ () ->

  xCreateGcontext (XDrawWindow root)
                  [XGCBackground bg_color,
                   XGCForeground fg_color] `thenIO` \ graphContext ->

  xDisplayForceOutput display `thenIO` \ () ->

  appendChan stdout ("Welcome to LOGO!\n" ++ prompt) exit $
  readChan stdin exit $ \userInput ->
  repLoop 
    (varEnvsInit,procEnvsInit,turtleInit)
    ((lines userInput,Lexer),
     (graphWindow,display,graphContext,bg_color,fg_color)) $
  xCloseDisplay display

-- Initial Environments --

varEnvsInit :: VarsType
varEnvsInit  = [[("GOODBYE",GoodBye)]]

-- all user-defined commands must have dummy entries
procEnvsInit :: ProcsType
procEnvsInit = (map (makeFakeProc)
                    [("XCOR",0),("YCOR",0),("GETANGLE",0),("GETPEN",0),
                     ("GETTURTLE",0),
                     ("SUM",2),("DIFFERENCE",2),("PRODUCT",2),("MOD",2),
                     ("DIV",2),("POWER",2),
                     ("AND",2),("OR",2),("NOT",1),
                     ("WORDP",1),("LISTP",1),("NUMBERP",1),("GREATER",2),
                     ("EQUAL",2),("LESS",2),
                     ("BUTFIRST",1),("FPUT",2),("CONCAT",2),
                     ("FIRST",1),("LAST",1),("WORD",-2),("LIST",-2),
                     ("SENTENCE",-2), ("USE",1)]):[]

turtleInit :: TurtleType
turtleInit   = (500 `div` 2,500 `div` 2,90,True,False)

-- makes a dummy procedure
makeFakeProc :: (NameType , Int) -> (NameType , ProcType)
makeFakeProc (name,num) = (name,(makeArgs num,[]))

makeArgs :: Int -> [NameType]
makeArgs n | n > 0     = "" : makeArgs (n-1)
           | otherwise = []

-- keep running Read-Eval-Print Loop until user types GoodBye
-- repLoop keeps running until user types "GoodBye", alternately
--   lexing, parsing, and evaluating each command
-- after a syntax error, the lex state is reset
repLoop :: EnvsType -> StateType -> IO () -> IO ()
repLoop e1 (inS1,gs1) end = 
  let fail1 msg (is1,ls1) = errorOutput msg $
                            repLoop e1 ((is1,Lexer),gs1) end
        -- parser fail continuation doesn't contain graphics state
      fail2 msg ((is2,ls2),gs2) = errorOutput msg $
                                  repLoop e1 ((is2,Lexer),gs1) end
        -- evaluator fail continuation does contain graphics state
  in
    parse [] inS1 fail1 $ \a ts inS2 ->
    if (null ts)
      then
        evaluate e1 a (inS2,gs1) fail2 $ \v e2 ((is3,ls3),gs3) ->
        output v end $
        repLoop e2 ((is3,Lexer),gs3) end
      else
        fail1 "Syntax error:  expected end of line" inS2
        -- repLoop will still be rerun

-- print error message
errorOutput :: String -> IO () -> IO ()
errorOutput error = appendChan stdout (error ++ prompt) abort

-- print expression value, exiting if GoodBye
output :: Value -> IO () -> IO () -> IO ()
output GoodBye end succ 
  = appendChan stdout "\nGoodbye!\n"abort end
output v       end succ
  = appendChan stdout ((valueToString v) ++ prompt) abort succ

prompt :: String
prompt = "\nLOGO> "



-------------------------------------------------------------------------------
module Evaluator where

{-

Evaluator takes an Abstract Syntax Tree and evaluates it in the
current environment, returning both the resultant value and the new
environment (as well as the updated state, of which only the user
input can actually be changed in the evaluator).

A value can be of one of six types:  integer, string, list, and
boolean, as well as null (for commands which don't return anything and
newly-declared local variables), and goodbye, which allows logo to
quit.

The environment consists of three parts.  The variable environment and
the procedure environment are separate (so that a name can refer both
to a variable and a procedure:  Logo syntax is such that there is
never any ambiguity) are both lists of name-value association lists.
Each association list representes a "local environment", with each
successive one being more "global", so that the last environment in
the list is the global environment.  Local environments are produced
by user-function invocations and removed at the end of those
invocations.

-}

import Lexer
import Parser
import Xlib

type NameType      = [Char]
type WordType      = [Char]
type Error         = [Char]

type StateType     = (InputState , GraphicsState)
type GraphicsState = (XWindow , XDisplay , XGcontext , XPixel , XPixel)
type EnvsType      = (VarsType,ProcsType,TurtleType)
type VarsType      = [[(NameType , Value)]]
type ProcsType     = [[(NameType , ProcType)]]
type TurtleType    = (Int , Int , Int , Bool , Bool)
type ProcType      = ([NameType] , ClauseType)

data Value         = Null
                   | Num Int
                   | Word WordType
                   | List ListType
                   | Boolean Bool
                   | GoodBye
                     deriving Text

data ListType      = NullList | Value :* ListType
                     deriving Text


type EvalFailType  = Error -> StateType -> IO ()
type EvalSuccType  = Value -> EnvsType -> StateType -> IO ()
type EvalResType   = StateType -> EvalFailType -> EvalSuccType -> IO ()
type EvaluateType  = EnvsType -> AST -> EvalResType


evaluate :: EvaluateType

evaluate (vs,p:ps,ttl) (To newName newProc)         ss fail succ
  = succ Null (vs,((newName,newProc):p):ps,ttl) ss
  -- procedures

evaluate e             (Read)                       ((i:is,ls),gs) fail succ
  = succ (List (makeReadList (lexerReadLine i))) e ((is,ls),gs)
  -- user input

evaluate e1            (Print [a])                  ss fail succ
  = evaluate e1 a ss fail $ \v e2 ss2 ->
    appendChan stdout ((valueToString v)++"\n") abort $
    succ Null e2 ss2
  -- user output

evaluate e             (Argument (Val (Word n)))    ss fail succ
  = lookup e n ss fail $ \v ->
    succ v e ss
  -- variable reference

evaluate e             (Argument (Val v))           ss fail succ
  = succ v e ss
  -- constant

evaluate e             (Argument (QuotedWordArg n)) ss fail succ
  = succ (Word n) e ss
  -- string constant

evaluate (v:vs,ps,ttl) (Local n)                    ss fail succ
  = succ Null (((n,Null):v):vs,ps,ttl) ss
  -- local variable declaraion
  -- local returns null, and sets the new local variable to null also

evaluate e             (ParseList l)                ss fail succ
  = succ (List l) e ss
  -- lists (also constant)

evaluate e             (Loop l cond insts)          ss fail succ
  = evalLoop l e cond insts ss fail succ
  -- loops

evaluate e             (If cond thens elses)        ss fail succ
  = evalIf e cond thens elses ss fail succ
  -- if-then[-eles] conditionals

evaluate e1            (Command name as1)           ss fail succ
  | ((na == length as1) || (na == -2))
    = evalArgs e1 as1 ss fail $ \e2 as2 ss2 ->
      apply name as2 e2 ss2 fail $ \v e3 ss3 ->
      succ v e3 ss3
  | na == -1
    = fail ("Function does not exist:  " ++ name) ss
  | otherwise
    = fail ("Wrong number of arguments to " ++ name) ss
  where na = numArgs e1 name
  -- function applications

evaluate e1            (Make n a)                   ss fail succ
  = evaluate e1 a ss fail $ \v e2 ss2 ->
    update e2 n v $ \e3 ->
    succ v e3 ss2
  -- assignment statements, which return the assigned value

evaluate e1            (Graphics name as1)          ss fail succ
  = evalArgs e1 as1 ss fail $ \e2 as2 ss2 ->
    doGraphics name as2 e2 ss2 fail $ \e3 ss3 ->
    succ Null e3 ss3
  -- side-effecting graphics statements, which all return null
-- end evaluate


-- evaluate a list of actual parameters, returning the corresponding
--   list of values
evalArgs :: EnvsType -> ParseArgs -> StateType -> EvalFailType ->
            (EnvsType -> EvalArgs -> StateType -> IO ()) -> IO ()
evalArgs e  []      ss fail succ
  = succ e [] ss
evalArgs e1 (a:as1) ss fail succ
  = evaluate e1 a ss fail $ \v e2 ss2 ->
    evalArgs e2 as1 ss2 fail $ \e3 as2 ss3 ->
    succ e3 (v:as2) ss3


-- evaluate a list of commands, returning the value of the last one
evalClause :: EnvsType -> ClauseType -> EvalResType
evalClause e  []     ss fail succ
  = succ Null e ss
evalClause e  (a:[]) ss fail succ
  = evaluate e a ss fail succ
evalClause e1 (a:as) ss fail succ
  = evaluate e1 a ss fail $ \v e2 ss2 ->
    evalClause e2 as ss2 fail succ

-- convert a lexed user-input list to a list constant
makeReadList :: [WordType] -> ListType
makeReadList []              = NullList
makeReadList (w:ws) = (Word w) :* (makeReadList ws)


-- Variable routines --

-- look up a variable reference in the variable environment
-- search the most-local environments first
-- return an error if not found
lookup :: EnvsType -> NameType -> StateType -> EvalFailType ->
          (Value -> IO ()) -> IO ()
lookup ([],ps,ttl)             name ss fail succ
  = fail ("Unbound variable:  " ++ name) ss
lookup ([]:vss,ps,ttl)         name ss fail succ
  = lookup (vss,ps,ttl) name ss fail succ
lookup (((n,v):vs):vss,ps,ttl) name ss fail succ
  | n == name = succ v
  | otherwise = lookup (vs:vss,ps,ttl) name ss fail succ

-- update the variable environment
-- replace the most-local occurrance  first; if none are found,
--   create a new variable and place it in the most-global environment
update :: EnvsType -> NameType -> Value -> (EnvsType -> IO ()) -> IO ()
update ([]:[],ps,ttl) name value succ
  = succ (((name,value):[]):[],ps,ttl)
update ([]:vss,ps,ttl) name value succ
  = update (vss,ps,ttl) name value $ \(vss2,ps2,ttl2) ->
    succ ([]:vss2,ps2,ttl2)
update (((n,v):vs):vss,ps,ttl) name value succ
  | n == name = succ (((n,value):vs):vss,ps,ttl)
  | otherwise = update (vs:vss,ps,ttl) name value $ \(vs2:vss2,ps2,ttl2) ->
                succ (((n,v):vs2):vss2,ps2,ttl2)


-- Control structures --

-- evaluate loops
evalLoop :: LoopType -> EnvsType -> ConditionType -> ClauseType ->
            EvalResType
evalLoop Do     = evalDo
evalLoop While  = evalWhile
evalLoop Repeat = evalRepeat

-- evaluate while statements
-- loop semantics:  evaluate condition; if true, evaluate clause, then loop
-- while returns null
evalWhile :: EnvsType -> ConditionType -> ClauseType -> EvalResType
evalWhile e1 cond insts ss fail succ
  = evalCond e1 cond ss fail $ \b e2 ss2 ->
    if b
      then
        evalClause e2 insts ss2 fail $ \v e3 ss3 ->
        evalWhile e3 cond insts ss3 fail succ
      else
        succ Null e2 ss2

-- evaluate do-while statements
-- loop semantics:  evaluate clause then evaluate condition; if true, loop
evalDo :: EnvsType -> ConditionType -> ClauseType -> EvalResType
evalDo e1 cond insts ss fail succ
  = evalClause e1 insts ss fail $ \v e2 ss2 ->
    evalCond e2 cond ss2 fail $ \b e3 ss3 ->
    if b
      then 
        evalDo e3 cond insts ss3 fail succ
      else
        succ Null e3 ss3

-- evaluate repeat statements
-- loop semantics:  evaluate loop number as n; evaluate clause n times
-- evaluate loop number and print error if it is negative or not an integer
evalRepeat :: EnvsType -> ConditionType -> ClauseType -> EvalResType
evalRepeat e1 cond insts ss fail succ
  = evaluate e1 cond ss fail $ \v e2 ss2 ->
    case v of
      Num n     -> if (n >= 0)
                     then doIterations e2 n insts ss2 fail succ
                     else fail "Repeat: Iteration count cannot be negative" ss2
      otherwise -> fail "Repeat:  Invalid iteration count" ss2

-- perform loop interations:  evaluate "insts" "n" times
doIterations :: EnvsType -> Int -> ClauseType -> EvalResType
doIterations e  0     insts ss fail succ
  = succ Null e ss
doIterations e1 (n+1) insts ss fail succ
  = evalClause e1 insts ss fail $ \v e2 ss2 ->
    doIterations e2 n insts ss2 fail succ

-- evaluates conditions and returns either true, false, or an error
evalCond :: EnvsType -> ConditionType -> StateType -> EvalFailType ->
            (Bool -> EnvsType -> StateType -> IO ()) -> IO ()
evalCond e1 cond ss fail succ 
  = evaluate e1 cond ss fail $ \v e2 ss2 ->
    case v of
      Boolean b -> succ b e2 ss2
      otherwise -> fail "Invalid condition" ss2

-- evaluate if-then[-else] statements
evalIf :: EnvsType -> ConditionType -> ClauseType -> ClauseType -> EvalResType
evalIf e1 cond thens elses ss fail succ
  = evalCond e1 cond ss fail $ \b e2 ss2 ->
    if b
      then evalClause e2 thens ss2 fail succ
      else evalClause e2 elses ss2 fail succ


-- Function application --

-- returns the number of arguments to a user-defined or built-in function
-- -1 means the function wasn't found
-- -2 means the function can take any number of arguments
numArgs :: EnvsType -> CommandName -> Int
numArgs (vs,[],ttl)     name
  = -1
numArgs (vs,[]:pss,ttl) name 
  = numArgs (vs,pss,ttl) name
numArgs (vs,((n,(formals,body)):ps):pss,ttl) name
  | inList ["WORD","SENTENCE","LIST"] name = -2
  | n == name                              = length formals
  | otherwise                              = numArgs (vs,ps:pss,ttl) name

-- apply a function to its arguments
-- mostly just decides if it's user-defined or built-in, then dispatches
apply :: CommandName -> EvalArgs -> EnvsType -> EvalResType
apply n as e ss fail succ
  | isBuiltIn n = applyPrimProc n as e ss fail succ
  | otherwise   = applyUserProc (getProc e n) as e ss fail succ



-- returns procedure "name" from the procedure environment
-- searches most-local environments first
-- precondition:  procedure does exist somewhere
getProc :: EnvsType -> CommandName -> ProcType
getProc (vss,[]:pss,ttl)        name
  = getProc (vss,pss,ttl) name
getProc (vs,((n,p):ps):pss,ttl) name
  | n == name = p
  | otherwise = getProc (vs,ps:pss,ttl) name

-- apply user function:
--   bind formal parameters
--   create local enviroments
--   evaluate body of function
--   destroy local environments
--   return value of body
applyUserProc :: ProcType -> EvalArgs -> EnvsType -> EvalResType
applyUserProc (formals,body) actuals e1 ss fail succ
  = bind formals actuals e1 $ \e2 ->
    evalClause e2 body ss fail $ \v (vs:vss,ps:pss,ts) ss2 ->
    succ v (vss,pss,ts) ss2

-- bind formal parameters to actuals in local environment
bind :: [NameType] -> EvalArgs -> EnvsType -> (EnvsType -> IO ()) -> IO ()
bind formals actuals (vss,pss,ttl) succ
  = succ ((zip formals actuals):vss,[]:pss,ttl)


-- Built-in functions --

-- returns true for built-in functions
isBuiltIn :: CommandName -> Bool
isBuiltIn = inList ["XCOR","YCOR","GETANGLE","GETPEN","GETTURTLE",
                    "SUM","DIFFERENCE","PRODUCT","MOD","DIV","POWER",
                    "AND","OR","NOT",
                    "WORDP","LISTP","NUMBERP","GREATER","EQUAL","LESS",
                    "BUTFIRST","FPUT","CONCAT",
                    "FIRST","LAST","WORD","LIST","SENTENCE", "USE"]


-- applies a built-in function to its arguments
applyPrimProc :: CommandName -> [Value] -> EnvsType -> EvalResType

applyPrimProc "XCOR"      [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = succ (Num x) (vs,ps,(x,y,a,p,t)) ss
applyPrimProc "YCOR"      [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = succ (Num y) (vs,ps,(x,y,a,p,t)) ss
applyPrimProc "GETANGLE"  [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = succ (Num a) (vs,ps,(x,y,a,p,t)) ss
applyPrimProc "GETPEN"    [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = succ (Boolean p) (vs,ps,(x,y,a,p,t)) ss
applyPrimProc "GETTURTLE" [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = succ (Boolean t) (vs,ps,(x,y,a,p,t)) ss

applyPrimProc "SUM"        [Num a , Num b] e ss fail succ
  = succ (Num (a+b)) e ss
applyPrimProc "DIFFERENCE" [Num a , Num b] e ss fail succ
  = succ (Num (a-b)) e ss
applyPrimProc "PRODUCT"    [Num a , Num b] e ss fail succ
  = succ (Num (a*b)) e ss
applyPrimProc "MOD"        [Num a , Num b] e ss fail succ
  = succ (Num (a `mod` b)) e ss
applyPrimProc "DIV"        [Num a , Num b] e ss fail succ
  = succ (Num (a `div` b)) e ss
applyPrimProc "POWER"      [Num a , Num b] e ss fail succ
  | b >= 0 = succ (Num (a^b)) e ss
  | otherwise = fail ("Negative exponent:  " ++ (show b)) ss

applyPrimProc "AND" [Boolean a , Boolean b] e ss fail succ
  = succ (Boolean (a && b)) e ss
applyPrimProc "OR"  [Boolean a , Boolean b] e ss fail succ
  = succ (Boolean (a || b)) e ss
applyPrimProc "NOT" [Boolean a]             e ss fail succ
  = succ (Boolean (not a)) e ss

applyPrimProc "WORDP"   [Word w]                e ss fail succ
  = succ (Boolean True) e ss
applyPrimProc "WORDP"   [v]                     e ss fail succ
  = succ (Boolean False) e ss
applyPrimProc "NUMBERP" [Num n]                 e ss fail succ
  = succ (Boolean True) e ss
applyPrimProc "NUMBERP" [v]                     e ss fail succ
  = succ (Boolean False) e ss
applyPrimProc "LISTP"   [List l]                e ss fail succ
  = succ (Boolean True) e ss
applyPrimProc "LISTP"   [v]                     e ss fail succ
  = succ (Boolean False) e ss
applyPrimProc "GREATER" [Num a , Num b]         e ss fail succ
  = succ (Boolean (a > b)) e ss
applyPrimProc "EQUAL"   [Num a , Num b]         e ss fail succ
  = succ (Boolean (a == b)) e ss
applyPrimProc "EQUAL"   [Word a , Word b]       e ss fail succ
  = succ (Boolean (a == b)) e ss
applyPrimProc "EQUAL"   [Boolean a , Boolean b] e ss fail succ
  = succ (Boolean (a == b)) e ss
applyPrimProc "LESS"    [Num a , Num b]         e ss fail succ
  = succ (Boolean (a < b)) e ss

applyPrimProc "BUTFIRST" [Word ""]                     e ss fail succ
  = succ (Word "") e ss
applyPrimProc "BUTFIRST" [Word (c:cs)]                 e ss fail succ
  = succ (Word cs) e ss
applyPrimProc "BUTFIRST" [List NullList]               e ss fail succ
  = succ (List NullList) e ss
applyPrimProc "BUTFIRST" [List (v :* vs)]              e ss fail succ
  = succ (List vs) e ss
applyPrimProc "FPUT"     [v , List l]                  e ss fail succ
  = succ (List (v :* l)) e ss
applyPrimProc "CONCAT"   [List l1 , List l2]           e ss fail succ
  = succ (List (listConcatenate l1 l2)) e ss
applyPrimProc "FIRST"    [Word (c:cs)]                 e ss fail succ
  = succ (Word (c:[])) e ss
applyPrimProc "FIRST"    [List (v :* vs)]              e ss fail succ
  = succ v e ss
applyPrimProc "LAST"     [Word (c:[])]                 e ss fail succ
  = succ (Word (c:[])) e ss
applyPrimProc "LAST"     [Word ""]                     e ss fail succ
  = succ Null e ss
applyPrimProc "LAST"     [Word (c:cs)]                 e ss fail succ
  = applyPrimProc "LAST" [(Word cs)] e ss fail succ
applyPrimProc "LAST"     [List (v :* NullList)]        e ss fail succ
  = succ v e ss
applyPrimProc "LAST"     [List (v :* vs)]              e ss fail succ
  = applyPrimProc "LAST" [(List vs)] e ss fail succ
applyPrimProc "WORD"     []                            e ss fail succ
  = succ (Word "") e ss
applyPrimProc "WORD"     ((Word w):ws)                 e ss fail succ
  = applyPrimProc "WORD" ws e ss fail $ \(Word wsc) e2 ss2 ->
    succ (Word (w ++ wsc)) e2 ss2
applyPrimProc "LIST"     (v:vs)                        e ss fail succ
  = applyPrimProc "LIST" vs e ss fail $ \(List l) e2 ss2 ->
    succ (List (v :* l)) e2 ss2
applyPrimProc "LIST"     []                            e ss fail succ
  = succ (List NullList) e ss
applyPrimProc "SENTENCE" []                            e ss fail succ
  = succ (List NullList) e ss
applyPrimProc "SENTENCE" ((List l):[])                 e ss fail succ
  = succ (List l) e ss
applyPrimProc "SENTENCE" ((List l):vs)                 e ss fail succ
  = applyPrimProc "SENTENCE" [List l] e  ss  fail $ \(List s1) e2 ss2 ->
    applyPrimProc "SENTENCE" vs       e2 ss2 fail $ \(List s2) e3 ss3 ->
    succ (List (listConcatenate s1 s2)) e3 ss3
applyPrimProc "SENTENCE" (v:vs)                        e ss fail succ
  = applyPrimProc "SENTENCE" vs e ss fail $ \(List ws) e2 ss2 ->
    succ (List (v :* ws)) e2 ss2

applyPrimProc "USE" [Word filename]                   
              e 
              ss@((ins, ls), gs)
              fail succ
  = readFile filename (\ _ -> fail ("Can't read file: " ++ filename) ss) 
    $ \filecontents ->
    useRepLoop e ((lines filecontents, Lexer), gs) 
               (\ msg s -> fail msg ss) $ \ v e s -> 
    succ v e ss
                         
applyPrimProc n          _                             _ ss fail _
  = fail ("Incorrect arguments:  " ++ n) ss

useRepLoop :: EnvsType -> EvalResType 
useRepLoop e  s@(([], ls), gs) fail succ = succ (Word "OK") e s
useRepLoop e1 s1@(inS1,gs1) fail succ = 
    parse [] inS1 (\ msg ins -> fail msg (ins, gs1)) $ \a ts inS2 ->
    if (null ts)
      then
        evaluate e1 a (inS2,gs1) fail $ \v e2 s3 ->
        useRepLoop e2 s3 fail succ
      else
        fail "Syntax error:  expected end of line" (inS2, gs1)



-- concatenates two lists
listConcatenate :: ListType -> ListType -> ListType
listConcatenate NullList  l2 = l2
listConcatenate (v :* l1) l2 = (v :* (listConcatenate l1 l2))


-- Graphics --

type EvalArgs = [Value]
type GraphEnv = (Int,Int,Int,Bool)

-- evaluates side-effecting graphics functions
-- note:  none of them return values
doGraphics :: CommandName -> EvalArgs -> EnvsType -> StateType -> 
              EvalFailType -> (EnvsType -> StateType -> IO ()) -> IO ()

doGraphics "HIDETURTLE" [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = hideTurtle x y a ss $
    succ (vs,ps,(x,y,a,p,False)) ss
  -- hide turtle, appropriately adjust environment

doGraphics "SHOWTURTLE" [] (vs,ps,(x,y,a,p,t)) ss fail succ
  = showTurtle x y a ss $
    succ (vs,ps,(x,y,a,p,True)) ss
  -- show turtle, appropriately adjust environment

doGraphics name as (vs,ps,(x,y,a,p,True)) ss fail succ
  = hideTurtle x y a ss $
    moveTurtle name as (x,y,a,p) ss $ \(x2,y2,a2,p2) ->
    showTurtle x2 y2 a2 ss $
    succ (vs,ps,(x2,y2,a2,p2,True)) ss
  -- executes graphics commands if turtle is shownn

doGraphics name as (vs,ps,(x,y,a,p,False)) ss fail succ
  = moveTurtle name as (x,y,a,p) ss $ \(x2,y2,a2,p2) ->
    succ (vs,ps,(x2,y2,a2,p2,False)) ss
  -- executes graphics commands if turtle is not shown

-- converts an integer to a float
toFloat :: Int -> Float
toFloat = fromInteger . toInteger

newmod a b = let c = a `mod` b
             in if (c < 0) then (c + b) else c

-- shows the turtle, but returns nothing
showTurtle :: Int -> Int -> Int -> StateType -> IO () -> IO ()
showTurtle x y a (is,(graphWindow,display,graphContext,bg,fg)) succ
  = let dx1 = round (12 * cos (toFloat a * pi/180))
        dx2 = round (4  * sin (toFloat a * pi/180))
	dy1 = round (12 * sin (toFloat a * pi/180))
	dy2 = round (4  * cos (toFloat a * pi/180))
    in 
    xDrawLine (XDrawWindow graphWindow) 
              graphContext
	      (XPoint x y) 
	      (XPoint (x-dx1-dx2) (y+dy1-dy2))
    `thenIO` \ () ->
    xDrawLine (XDrawWindow graphWindow)
              graphContext
	      (XPoint x y)
	      (XPoint (x-dx1+dx2) (y+dy1+dy2))
    `thenIO` \ () ->
    xDrawLine (XDrawWindow graphWindow)
              graphContext
	      (XPoint (x-dx1-dx2) (y+dy1-dy2))
	      (XPoint (x-dx1+dx2) (y+dy1+dy2))
    `thenIO` \ () ->
    xDisplayForceOutput display
    `thenIO_`
    succ

-- hides the turtle, but returns nothing
hideTurtle :: Int -> Int -> Int -> StateType -> IO () -> IO ()
hideTurtle x y a (is,(graphWindow,display,graphContext,bg,fg)) succ
  = xUpdateGcontext graphContext [XGCForeground bg] 
    `thenIO_`
    (showTurtle x y a (is,(graphWindow,display,graphContext,bg,fg)) $
    (xUpdateGcontext graphContext [XGCForeground fg]
    `thenIO_`
    succ))

-- performs all graphics commands that don't involve hiding/showing 
--   the turtle
moveTurtle :: CommandName -> EvalArgs -> GraphEnv -> StateType ->
              (GraphEnv -> IO ()) -> IO ()
moveTurtle "SETXY"       [Num xp,Num yp] (x,y,a,p) ss succ
  = succ (xp,yp,a,p)

-- move the turtle forward "d" times, drawing a line if pen is down
moveTurtle "FORWARD"     [Num d]         (x,y,a,p) 
           (is,(graphWindow,display,graphContext,fg,bg)) succ
  = let xp = x + round (toFloat d * cos (toFloat a * pi/180))
        yp = y - round (toFloat d * sin (toFloat a * pi/180)) in 
     (if p 
        then (xDrawLine (XDrawWindow graphWindow) 
	                graphContext
			(XPoint x y) 
			(XPoint xp yp))
        else returnIO ()) `thenIO` \ () ->
     xDisplayForceOutput display `thenIO` \ () ->
     succ (xp,yp,a,p)

-- move the turtle backward "d" pixels, drawing a line if pen is down
moveTurtle "BACKWARD"    [Num d]         (x,y,a,p) ss succ
  = moveTurtle "FORWARD" [Num (-d)] (x,y,a,p) ss succ

-- rotate turtle to "ap" degrees from facing due east
moveTurtle "SETANGLE"    [Num ap]        (x,y,a,p) ss succ
  = succ (x,y,ap,p)

-- rotate turtle counterclockwise "ap" degrees
moveTurtle "LEFT"        [Num ap]        (x,y,a,p) ss succ
  = succ (x,y, (a + ap) `newmod` 360 ,p)

-- rotate turtle clockwise "ap" degrees
moveTurtle "RIGHT"       [Num ap]        (x,y,a,p) ss succ
  = succ (x,y, (a - ap) `newmod` 360 ,p)

-- pick pen up
moveTurtle "PENUP"       []              (x,y,a,p) ss succ
  = succ (x,y,a,False)

-- put pen down
moveTurtle "PENDOWN"     []              (x,y,a,p) ss succ
  = succ (x,y,a,True)

-- clear screen but don't otherwise alter turtle state
moveTurtle "CLEARSCREEN" []              (x,y,a,p) 
           (is,(graphWindow,display,graphContext,bg,fg)) succ
  = xClearArea graphWindow (XRect 0 0 500 500) True 
    `thenIO` \() ->
    xDisplayForceOutput display 
    `thenIO` \() ->
    succ (x,y,a,p)

-- pick pen up and reset turtle
moveTurtle "CLEAN"       []              (x,y,a,p) 
           (is,(graphWindow,display,graphContext,bg,fg)) succ
  = xClearArea graphWindow (XRect 0 0 500 500) True 
    `thenIO` \() ->
    xDisplayForceOutput display 
    `thenIO` \() ->
    succ (500 `div` 2,500 `div` 2,90,True)

-- do nothing if arguments are incorrect
moveTurtle _ _ e _ succ = succ e


-- valueToString, etc. --

-- convert a value to a string
valueToString :: Value -> String
valueToString (Word w)        = w
valueToString (Num n)         = show n
valueToString (Boolean True)  = "TRUE"
valueToString (Boolean False) = "FALSE"
valueToString Null            = ""
valueToString (List l)        = "[" ++ (listToString l) ++ "]"
valueToString GoodBye         = "Don't play around with this variable!"

-- convert a list to a string
listToString :: ListType -> String
listToString NullList        = ""
listToString (v :* NullList) = valueToString v
listToString (v :* l)        = (valueToString v) ++ " " ++ (listToString l)



-------------------------------------------------------------------------------
module Lexer where

{-

Lexer takes as input a line from standard input and returns an ordered
pair containing the translation of that list into tokens as well as
the current state of the lexer (how many parentheses and brackets are
still open).  The state is necessary because some commands may take
multiple lines, so a bracket (say) may be left open on one line to be
closed later on.

All unmatched close brackets and parentheses are treated as spaces
(and therefore ignored).

The method for tokenizing commands is:

  All words are delimited by spaces, parenthesis, or brackets.

  All words beginning with a double quote are returned as quoted words
  rather than normal words.

  Any character preceded by a backslash is taken as is, rather than
  tokenized normally.

  All words are translated to upper case..

The method for tokenizing user input is:

  All words are delimited by spaces and translated to upper case.
  
-}

import Parser
import Evaluator


data LexState = Lexer | LexerBracket Int LexState | LexerParen Int LexState
                deriving Text

type LexerType = [Char] -> ([Token] , LexState)

data Token   = OpenBracket 
             | CloseBracket 
             | OpenParen 
             | CloseParen
             | QuotedWord WordType
             | Normal WordType     deriving (Text,Eq)


-- call appropriate lex procedure depending upon the current lex state
lexDispatch :: LexState -> LexerType
lexDispatch (Lexer)            = lexer
lexDispatch (LexerBracket n s) = lexerBracket n s
lexDispatch (LexerParen n s)   = lexerParen n s


-- handle commands
lexer :: LexerType
lexer []       = ([] , Lexer)
lexer (' ':cs) = lexer cs
lexer ('[':cs) = let (ts , s) = lexerBracket 1 (Lexer) cs
                 in (OpenBracket : ts , s)
lexer ('(':cs) = let (ts , s) = lexerParen 1 (Lexer) cs
                 in (OpenParen : ts , s)
lexer (')':cs) = lexer cs
lexer (']':cs) = lexer cs
lexer ('"':cs) = let (t , cs2) = lexerWord (isDelimiter) cs
                     (ts , s)  = lexer cs2
                 in ((QuotedWord (upWord t)):ts , s)
lexer cs       = let (t , cs2) = lexerWord (isDelimiter) cs
                     (ts , s)  = lexer cs2
                 in ((Normal (upWord t)):ts , s)

lexerWord :: (Char -> Bool) -> [Char] -> (WordType , [Char])
lexerWord endCond []
  = ([] , [])
lexerWord endCond (c:cs)
  | c == '\\' = if cs == []
                  then ("\\" , cs)
                  else 
                    let (t , cs2) = lexerWord endCond (tail cs)
                    in ((head cs):t , cs2)
  | endCond c = ([] , (c:cs))
  | otherwise = let (t , cs2) = lexerWord endCond cs
                in ((toUpper c):t , cs2)


-- performs lexing inside brackets
lexerBracket :: Int -> LexState -> LexerType
lexerBracket n s []
  = ([] , LexerBracket n s)
lexerBracket n s (' ':cs)
  = lexerBracket n s cs
lexerBracket 1 s (']':cs)
  = let (ts , s2) = lexDispatch s cs
    in (CloseBracket:ts , s2)
lexerBracket n s (']':cs)
  = let (ts , s2) = lexerBracket (n-1) s cs
    in (CloseBracket:ts , s2)
lexerBracket n s ('[':cs)
  = let (ts , s2) = lexerBracket (n+1) s cs
    in (OpenBracket:ts , s2)
lexerBracket n s ('(':cs)
  = let (ts , s2) = lexerParen 1 (LexerBracket n s) cs
    in (OpenParen:ts , s2)
lexerBracket n s (')':cs)
  = lexerBracket n s cs
lexerBracket n s cs
  = let (t , cs2) = lexerWord (isDelimiter) cs
        (ts , s2) = lexerBracket n s cs2
    in ((Normal (upWord t)):ts , s2)


-- performs lexing inside parentheses
lexerParen :: Int -> LexState -> LexerType
lexerParen n s []
  = ([] , LexerParen n s)
lexerParen n s (' ':cs)
  = lexerParen n s cs
lexerParen 1 s (')':cs)
  = let (ts , s2) = lexDispatch s cs
    in (CloseParen:ts , s2)
lexerParen n s (')':cs)
  = let (ts , s2) = lexerParen (n-1) s cs
    in (CloseParen:ts , s2)
lexerParen n s ('(':cs)
  = let (ts , s2) = lexerParen (n+1) s cs
    in (OpenParen:ts , s2)
lexerParen n s ('[':cs)
  = let (ts , s2) = lexerBracket 1 (LexerParen n s) cs
    in (OpenBracket:ts , s2)
lexerParen n s (']':cs)
  = lexerParen n s cs
lexerParen n s ('"':cs)
  = let (t , cs2) = lexerWord (isDelimiter) cs
        (ts , s2) = lexerParen n s cs2
    in ((QuotedWord (upWord t)):ts , s2)
lexerParen n s cs
  = let (t , cs2) = lexerWord (isDelimiter) cs
        (ts , s2) = lexerParen n s cs2
    in ((Normal (upWord t)):ts , s2)


-- returns true for delimiters
isDelimiter :: Char -> Bool
isDelimiter = inList " []()"

-- returns true of p is in cs
inList :: (Eq a) => [a] -> a -> Bool
inList [] p     = False
inList (c:cs) p = (c == p) || (inList cs p)


-- handle user input
lexerReadLine :: [Char] -> [WordType]
lexerReadLine []
  = []
lexerReadLine (' ':cs)
  = lexerReadLine cs
lexerReadLine cs
  = let (firstWord,restOfWords) = span (/= ' ') cs 
    in (upWord firstWord) : lexerReadLine restOfWords

-- translate a word to upper case
upWord :: WordType -> WordType
upWord = map (toUpper)



-------------------------------------------------------------------------------
module Parser where

{-

Parser takes a list of tokens, the input state, and fail and success
continuations and returns an Abstract Syntax Tree, the remaining
tokens (hopefully none), and the new input state.  The input state
will be changed every time Parser runs out of tokens:  it simply grabs
(and lexes) the next line of user-input.  It therefore doesn't return
anything until the entire AST has been be read in, even if it spans
several lines, though parse may catch some errors before all lines
have been input.  In this case, it ceases taking input and returns the
error.

An Abstract Syntax Tree represents one command, and breaks those
commands into Ifs, Loops, Tos, Locals, Makes, Reads, Prints,
Constants, List constants, Graphics commands (which produce
side-effects), and function applications.  All built-in commands that
don't fit into one of those categories are lumped into function
applications along with user-defined functions.  Each type of AST is
parsed into subcommands, subclauses (lists of commands), command
arguments (also subcommands), and any other values that will be
immediately-evaluatable (such as function names).

-}


import Lexer
import Evaluator


type CommandName     = [Char]
type ClauseType      = [AST]
type ConditionType   = AST

type ParseArgs       = [AST]

data ArgType         = Val Value | QuotedWordArg WordType
                       deriving Text

data AST             = ParseList ListType
                     | If ConditionType ClauseType ClauseType
                     | Loop LoopType ConditionType ClauseType
                     | To NameType ProcType
                     | Make NameType AST
                     | Local NameType
                     | Read
                     | Print ParseArgs
                     | Argument ArgType
                     | Graphics CommandName ParseArgs
                     | Command CommandName ParseArgs      deriving Text

data LoopType        = Do | While | Repeat
                       deriving Text

type ParseFailType   = Error -> InputState -> IO ()
type ParseType       = [Token] -> InputState -> ParseFailType ->
                       (AST -> [Token] -> InputState -> IO ()) -> IO ()
type ParseClauseType = [Token] -> InputState -> ParseFailType -> 
                       (ClauseType -> [Token] -> InputState -> IO ()) -> IO ()

type InputState      = ([[Char]] , LexState)

parse :: ParseType

parse [] (i:is , ls) fail succ 
  = let (ts , ls2) = lexDispatch ls i
    in parse ts (is , ls2) fail succ  

parse ((QuotedWord s) : ts) inS fail succ 
  = succ (Argument (QuotedWordArg s)) ts inS

parse ((Normal s) : ts) inS fail succ
  = succ (Argument (Val (process s))) ts inS

parse (OpenParen : []) (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parse (OpenParen:ts) (is,ls2) fail succ

parse (OpenParen : (Normal t) : ts) inS fail succ
  | t == "TO"    = makeProc ts inS fail succ
  | t == "MAKE"  = makeMake ts inS fail succ
  | t == "LOCAL" = makeLocal ts inS fail succ
  | t == "READ"  = makeRead ts inS fail succ
  | t == "PRINT" = makePrint ts inS fail succ
  | t == "IF"    = makeIf ts inS fail succ
  | isLoop t     = makeLoop t ts inS fail succ
  | isGraphics t = makeGraphics t ts inS fail succ
  | otherwise    = makeCommand t ts inS fail succ

parse (OpenBracket : ts) inS fail succ
  = parseList ts inS fail succ

parse ts inS@([], _) _ succ = succ (Argument (Val (Word "GOODBYE"))) ts inS

parse _ inS fail _
  = fail "Syntax error" inS


-- returns true for all loop names
isLoop :: CommandName -> Bool
isLoop = inList ["DO","WHILE","REPEAT"]

-- returns true for all side-effecting graphics command names
isGraphics :: CommandName -> Bool
isGraphics = inList ["FORWARD","BACKWARD","LEFT","RIGHT",
                     "SETXY","SETANGLE","PENUP","PENDOWN",
                     "HIDETURTLE","SHOWTURTLE","CLEARSCREEN","CLEAN"]

-- Parse lists --

-- parses a list constant
parseList :: ParseType
parseList []                  (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseList ts (is,ls2) fail succ
parseList (CloseBracket:ts)   inS fail succ
  = succ (ParseList NullList) ts inS
parseList (OpenBracket:ts)    inS fail succ
  = parseList ts inS fail $ \(ParseList l1) ts2 inS2 ->
    parseList ts2 inS2 fail $ \(ParseList l2) ts3 inS3 ->
    succ (ParseList ((List l1) :* l2)) ts3 inS3
parseList ((Normal w):ts)     inS fail succ
  = parseList ts inS fail $ \(ParseList l) ts2 inS2 ->
    succ (ParseList ((process w) :* l)) ts2 inS2
parseList (OpenParen:ts)      inS fail succ
  = parseList ts inS fail $ \(ParseList l) ts2 inS2 ->
    succ (ParseList ((Word "(") :* l)) ts2 inS2
parseList (CloseParen:ts)     inS fail succ
  = parseList ts inS fail $ \(ParseList l) ts2 inS2 ->
    succ (ParseList ((Word ")") :* l)) ts2 inS2
parseList ((QuotedWord w):ts) inS fail succ
  = parseList ts inS fail $ \(ParseList l) ts2 inS2 ->
    succ (ParseList ((Word w) :* l)) ts2 inS2


-- parses constant values, distinguishing words from integers and booleans
process :: WordType -> Value
process "TRUE"  = Boolean True
process "FALSE" = Boolean False
process ('-':w)
  | all isDigit w = Num (- (stringToNum (reverse w)))
  | otherwise     = Word ('-':w)
process w
  | all isDigit w = Num (stringToNum (reverse w))
  | otherwise     = Word w

-- converts a string to a positive integer
stringToNum :: String -> Int
stringToNum (d:[]) = charToDigit d
stringToNum (d:ds) = (charToDigit d) + 10 * stringToNum ds

-- converts a character to a digit
charToDigit :: Char -> Int
charToDigit c = ord c - ord '0'


-- Parse command statements --

-- parses commands
-- format:  (<name> <arg1> <arg2> ...)
makeCommand :: CommandName -> ParseType
makeCommand n ts inS fail succ
  = parseArgs CloseParen ts inS fail $ \as ts2 inS2 ->
    succ (Command n as) ts2 inS2


-- parses a list of commands that are terminated by token "term""
parseArgs :: Token -> ParseClauseType
parseArgs term [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseArgs term ts (is,ls2) fail succ
parseArgs term (t:ts) inS fail succ
  | t == term = succ [] ts inS
  | otherwise = parse (t:ts) inS fail $ \a ts2 inS2 ->
                parseArgs term ts2 inS2 fail $ \as ts3 inS3 ->
                succ (a:as) ts3 inS3


-- Parse I/O statements --

-- parses read statements
-- format:  (READ)
makeRead :: ParseType
makeRead (CloseParen:ts) inS fail succ
  = succ Read ts inS
makeRead _ inS fail _
  = fail "Read:  too many arguments" inS

-- parses print statements
-- format:  (PRINT <arg1>)
makePrint :: ParseType
makePrint ts inS fail succ
  = parseArgs CloseParen ts inS fail $ \as ts2 inS2 ->
    if (length as) == 1
      then succ (Print as) ts2 inS2
      else fail "Print:  too many arguments" inS



-- Parse TO statements --


-- parses to statements
-- format:  (TO <name> <fpname1> <fpname2> ... <clause>)
-- note:  all formal parameter names must begin with a colon
makeProc :: ParseType
makeProc [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in makeProc ts (is,ls2) fail succ
makeProc ((Normal t):ts) inS fail succ
  = parseFormals ts inS fail $ \p ts2 inS2 -> 
    getParen ts2 inS2 fail $ \ts3 inS3 ->
    succ (To t p) ts3 inS3
makeProc _ inS fail _
  = fail "Invalid procedure name" inS

-- parses the formal parameters
-- takes all words beginning with a colon, and assumes everything
--   after that is part of the body
parseFormals :: [Token] -> InputState -> ParseFailType ->
                (([NameType] , ClauseType) -> [Token] -> InputState -> IO ())
                -> IO ()
parseFormals [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseFormals ts (is,ls2) fail succ
parseFormals (OpenBracket:ts) inS fail succ
  = parseClause (OpenBracket:ts) inS fail $ \pb ts2 inS2 ->
    succ ([],pb) ts2 inS2
parseFormals ((Normal (':':c:cs)):ts) inS fail succ
  = parseFormals ts inS fail $ \(formals,pb) ts2 inS2 ->
    succ ((':':c:cs):formals , pb) ts2 inS2
parseFormals ts inS fail succ
  = parseClause ts inS fail $ \pb ts2 inS2 ->
    succ ([],pb) ts2 inS2


-- Parse MAKE statements --

-- parses make statements
-- format:  (MAKE <name> <arg>)
-- note:  <name> must be quoted
makeMake :: ParseType
makeMake [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in makeMake ts (is,ls2) fail succ
makeMake ((QuotedWord s):ts) inS fail succ
  = parse ts inS fail $ \a ts2 inS2 ->
    getParen ts2 inS2 fail $ \ts3 inS3 ->
    succ (Make s a) ts3 inS3
makeMake _ inS fail _
  = fail "Make:  Improper variable name" inS


-- Parse LOCAL statements --

-- parses local statements
-- format:  (LOCAL <name>)
-- note:  <name> must be quoted  
makeLocal :: ParseType
makeLocal [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in makeLocal ts (is,ls2) fail succ
makeLocal (t:[]) (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in makeLocal (t:ts) (is,ls2) fail succ
makeLocal ((QuotedWord s):CloseParen:ts) inS fail succ
  = succ (Local s) ts inS
makeLocal _ inS fail _
  = fail "Local:  improper variable name" inS


-- Parse IF statements --

-- parses if-then and if-then-else statements
-- format:  (IF <cond> then <clause> [else <clause>])
makeIf :: ParseType
makeIf [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in makeIf ts (is,ls2) fail succ
makeIf ts inS fail succ
  = parse ts inS fail $ \cond ts2 inS2 ->
    parseThen ts2 inS2 fail $ \thens elses ts3 inS3 ->
    getParen ts3 inS3 fail $ \ts4 inS4 ->
    succ (If cond thens elses) ts4 inS4


-- parses then clauses
parseThen :: [Token] -> InputState -> ParseFailType ->
             (ClauseType -> ClauseType -> [Token] -> InputState -> IO ()) -> 
             IO ()
parseThen [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseThen ts (is,ls2) fail succ
parseThen ((Normal "THEN"):ts) inS fail succ
  = parseClause ts inS fail $ \thens ts2 inS2 ->
    parseElse ts2 inS2 fail $ \elses ts3 inS3 ->
    succ thens elses ts3 inS3
parseThen _ inS fail _
  = fail "IF:  improper THEN clause" inS

-- parses (optional) else clauses
parseElse :: ParseClauseType
parseElse [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseElse ts (is,ls2) fail succ
parseElse (CloseParen:ts) inS fail succ
  = succ [] (CloseParen:ts) inS
parseElse ((Normal "ELSE"):ts) inS fail succ
  = parseClause ts inS fail succ
parseElse _ inS fail _
  = fail "IF:  improper ELSE clause" inS

-- parses clauses
-- a clause is either a list of commands enclosed in brackets, or a
--   single command
parseClause :: ParseClauseType
parseClause [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseClause ts (is,ls2) fail succ
parseClause (OpenBracket:ts) inS fail succ
  = parseArgs CloseBracket ts inS fail succ
parseClause ts inS fail succ
  = parse ts inS fail $ \a ts2 inS2 ->
    succ [a] ts2 inS2


-- Parse Loop Statements --

-- parses loop statements
-- basically a dispatcher for other parse functions
makeLoop :: NameType -> ParseType
makeLoop "DO"     = makeDo
makeLoop "WHILE"  = makeWhile
makeLoop "REPEAT" = makeRepeat

-- parses do statements
-- format:  (DO <clause> WHILE <cond>)
makeDo :: ParseType
makeDo ts inS fail succ
  = parseClause ts inS fail $ \insts ts2 inS2 ->
    parseWhileCond ts2 inS2 fail $ \cond ts3 inS3 ->
    getParen ts3 inS3 fail $ \ts4 inS4 ->
    succ (Loop Do cond insts) ts4 inS4

-- parses while conditions (both in while and do-while loops)
-- a condition is simply a command that (hopefully) returns a boolean
parseWhileCond :: ParseType
parseWhileCond [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseWhileCond ts (is,ls2) fail succ
parseWhileCond ((Normal "WHILE"):ts) inS fail succ
  = parse ts inS fail succ

-- parses while statements
-- format:  (WHILE <cond> <clause>)
makeWhile :: ParseType
makeWhile ts inS fail succ
  = parse ts inS fail $ \cond ts2 inS2 ->
    parseClause ts2 inS fail $ \insts ts3 inS3 ->
    getParen ts3 inS3 fail $ \ts4 inS4 ->
    succ (Loop While cond insts) ts4 inS4

-- parses repeat statements
-- format:  (REPEAT <num> TIMES <clause>)
-- note:  <num> is simply a command that (hopefully) returns an integer
makeRepeat :: ParseType
makeRepeat ts inS fail succ
  = parse ts inS fail $ \num ts2 inS2 ->
    parseRepeatBody ts2 inS fail $ \insts ts3 inS3 ->
    getParen ts3 inS3 fail $ \ts4 inS4 ->
    succ (Loop Repeat num insts) ts4 inS4

-- parses repeat body (just a clause)
parseRepeatBody :: ParseClauseType
parseRepeatBody [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in parseRepeatBody ts (is,ls2) fail succ
parseRepeatBody ((Normal "TIMES"):ts) inS fail succ
  = parseClause ts inS fail succ
parseRepeatBody _ inS fail _
  = fail "Repeat:  invalid format" inS


-- Parse Graphics Statements --

-- parses all side-effecting graphics statements
makeGraphics :: CommandName -> ParseType
makeGraphics n ts inS fail succ
  = parseArgs CloseParen ts inS fail $ \as ts2 inS2 ->
    succ (Graphics n as) ts2 inS2

-- Parse Trailing Parenthesis --

-- parses the closing paren terminating most commands
getParen :: [Token] -> InputState -> ParseFailType ->
            ([Token] -> InputState -> IO ()) -> IO ()
getParen [] (i:is,ls) fail succ
  = let (ts,ls2) = lexDispatch ls i
    in getParen ts (is,ls) fail succ
getParen (CloseParen:ts) inS fail succ
  = succ ts inS
getParen _ inS fail _
  = fail "Expected )" inS

