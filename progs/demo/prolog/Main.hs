--
-- Prolog interpreter top level module
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Main(main) where

import PrologData
import Parse
import Interact
import Subst
import Engine
import Version

--- Command structure and parsing:

data Command = Fact Clause | Query [Term] | Show | Error | Quit | NoChange

command :: Parser Command
command  = just (sptok "bye" `orelse` sptok "quit") `do` (\quit->Quit)
               `orelse`
           just (okay NoChange)
               `orelse`
           just (sptok "??") `do` (\show->Show)
               `orelse`
           just clause `do` Fact
               `orelse`
           just (sptok "?-" `seq` termlist) `do` (\(q,ts)->Query ts)
               `orelse`
           okay Error

--- Main program read-solve-print loop:

signOn           :: String
signOn            = "Mini Prolog Version 1.5 (" ++ version ++ ")\n\n"

main             :: Dialogue
main              = --echo False abort
                    (appendChan stdout signOn abort
                    (appendChan stdout ("Reading " ++ stdlib ++ "...") abort
                    (readFile stdlib
                      (\fail -> appendChan stdout "not found\n" abort
                                (interpreter ""))
                      (\lib  -> appendChan stdout "done\n"      abort
                                (interpreter lib))
                    )))

stdlib           :: String
stdlib            = "$HASKELL/progs/demo/prolog/stdlib"

interpreter      :: String -> Dialogue
interpreter lib   = readChan stdin abort
                    (\inn -> appendChan stdout (loop startDb inn) abort done)
                    where startDb = foldl addClause emptyDb clauses
                          clauses = [r | ((r,""):_)<-map clause (lines lib)]

loop             :: Database -> String -> String
loop db           = readln "> " (exec db . fst . head . command)

exec             :: Database -> Command -> String -> String
exec db (Fact r)  = skip                              (loop (addClause db r))
exec db (Query q) = demonstrate db q
exec db Show      = writeln (show db)                 (loop db)
exec db Error     = writeln "I don't understand\n"    (loop db)
exec db Quit      = writeln "Thank you and goodbye\n" end
exec db NoChange  = skip                              (loop db)

--- Handle printing of solutions etc...

solution      :: [Id] -> Subst -> [String]
solution vs s  = [ show (Var i) ++ " = " ++ show v
                                | (i,v) <- [ (i,s i) | i<-vs ], v /= Var i ]

demonstrate     :: Database -> [Term] -> Interactive
demonstrate db q = printOut (map (solution vs) (prove db q))
 where vs               = (nub . concat . map varsIn) q
       printOut []      = writeln "no.\n"     (loop db)
       printOut ([]:bs) = writeln "yes.\n"    (loop db)
       printOut (b:bs)  = writeln (doLines b) (nextReqd bs)
       doLines          = foldr1 (\xs ys -> xs ++ "\n" ++ ys)
       nextReqd bs      = writeln " "
                            (readch (\c->if c==';'
                                           then writeln ";\n" (printOut bs)
                                           else writeln "\n"  (loop db)) "")

--- End of Main.hs
