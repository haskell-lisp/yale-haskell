-- Page 0    Introduction

This is a programming supplement to `A Gentle Introduction to Haskell'
by Hudak and Fasel.  This supplement augments the tutorial by
providing executable Haskell programs which you can run and
experiment with.  All program fragments in the tutorial are
found here, as well as other examples not included in the tutorial.


Using This Tutorial

You should have a copy of both the `Gentle Introduction' and the
report itself to make full use of this tutorial.  Although the
`Gentle Introduction' is meant to stand by itself, it is often easier
to learn a language through actual use and experimentation than by
reading alone.  Once you finish this introduction, we recommend that
you proceed section by section through the `Gentle Introduction' and
after having read each section go back to this online tutorial.  You
should wait until you have finished the tutorial before attempting to
read the report.  We assume that you are familiar with the basics of
Emacs and that Haskell has been installed at your site.

This tutorial does not assume any familiarity with Haskell or other
functional languages.  However, knowledge of almost-functional
languages such as ML or Scheme is very useful.  Throughout the
online component of this tutorial, we try to relate Haskell to
other programming languages and clarify the written tutorial through
additional examples and text.


Organization of the Online Tutorial

This online tutorial is divided into a series of pages.  Each page
covers one or more sections in the written tutorial.  You can use
special Emacs commands to move back and forth through the pages of the
online tutorial.  Each page is a single Haskell program.  Comments in
the program contain the text of the online tutorial.  You can modify
the program freely (this will not change the underlying tutorial
file!) and ask the system to print the value of expressions defined in
the program.

At the beginning of each page, the sections covered by the page are
listed.  In addition, the start of each individual section is
marked within each page.  Emacs commands can take you directly to a
specific page or section in the tutorial.

To create useful, executable examples of Haskell code, some language
constructs need to be revealed well before they are explained in the
tutorial.  We attempt to point these out when they occur.  Some
small changes have been made to the examples in the written tutorial;
these are usually cosmetic and should be ignored.  Don't feel you have
to understand everything on a page before you move on -- many times
concepts become clearer as you move on and can relate them to other
aspect of the language.

Each page of the tutorial defines a set of variables.  Some of
these are named e1, e2, and so on.  These `e' variables are the ones
which are meant for you to evaluate as you go through the tutorial.
Of course you may evaluate any other expressions or variables you wish.


The Haskell Report

While the report is not itself a tutorial on the Haskell language, it
can be an invaluable reference to even a novice user.  A very
important feature of Haskell is the prelude.  The prelude is a
rather large chunk of Haskell code which is implicitly a part of every
Haskell program.  Whenever you see functions used which are not
defined in the current page, these come from the Prelude.  Appendix A
of the report lists the entire Prelude; the index has an entry for
every function in the Prelude.  Looking at the definitions in the
Prelude is sometimes necessary to fully understand the programs in
this tutorial.

Another reason to look at the report is to understand the syntax of
Haskell.  Appendix B contains the complete syntax for Haskell.  The
tutorial treats the syntax very informally; the precise details are
found only in the report.


The Yale Haskell System

This version of the tutorial runs under version Y2.0 of Yale Haskell.
The Yale Haskell system is an interactive programming environment for
the Haskell language.  The system is best used in conjunction with the
Emacs editor.  Yale Haskell is available free of change via ftp.


Using the Compiler

Yale Haskell runs as a subprocess under Emacs.  While many commands
are available to the Yale Haskell user, a single command is the
primary means of communicating with the compiler: C-c e.  This command
evaluates and prints an expression in the context of the program on
the screen.  Here is what this command does:

a) You are prompted for an expression in the minibuffer.  You can
use M-p or M-n to move through a ring of previous inputs.

b) If an inferior Haskell process is not running, a buffer named *haskell*
is created and the Haskell compiler is started up.  The *haskell* buffer
pops onto your screen.

c) If the program in the current page of the tutorial has not yet been
compiled or the page has been modified after its most recent
compilation, the entire page is compiled.  This may result in a short delay.

d) If there are no errors in the program, the expression entered in
step a) is compiled in the context of the program.  Any value defined
in the current page can be referenced.

e) If there are no errors in the expression, its value is printed in
the *haskell* buffer.

There are also a few other commands you can use.  C-c i interrupts
the Haskell program.  Some tight loops cannot be interrupted; in this
case you will have to kill the Haskell process.    C-c q exits the Haskell
process.


Emacs Commands Used by the Tutorial

These commands are specific to the tutorial.  The tutorial is entered
using M-x haskell-tutorial and is exited with C-c q.  To move among
the pages of the tutorial, use

C-c C-f  -- go forward 1 page
C-c C-b  -- go back 1 page
M-x ht-goto-page    - goto a specific page of the tutorial
M-x ht-goto-section - goto a specific section of the tutorial

Each page of the tutorial can be modified without changing the
underlying text of the tutorial.  Changes are not saved as you go
between pages.  To revert a page to its original form use C-c C-l.

You can get help regarding the Emacs commands with C-c ?.

Summary of Emacs commands used by the tutorial:
  M-x haskell-tutorial  - start the tutorial
  C-c C-f  - Go to the next page of the tutorial program
  C-c C-b  - Go back to the previous page of the tutorial program
  C-c C-l  - Restore the current page to its original form
  C-c e    - Evaluate a Haskell expression
  C-c i    - Interrupt a running Haskell program
  C-c ?    - Shows a help message
  M-x ht-goto-page    - goto a specific page of the tutorial
  M-x ht-goto-section - goto a specific section of the tutorial


You are now ready to start the tutorial.  Start by reading the `Gentle
Introduction' section 1 then proceed through the online tutorial using
C-c C-f to advance to the next page.  You should read about each topic
first before turning to the associated programming example in the
online tutorial.


-- Page 1   Section 2

-- Section 2   Values, Types, and Other Goodies

-- Haskell uses `--' to designate end of line comments.  We use these
-- throughout the tutorial to place explanatory text in the program.

-- Remember to use C-c e to evaluate expressions, C-c ? for help.

-- All Haskell programs must start with a module declaration.  Ignore this
-- for now.

module Test(Bool) where

-- We will start by defining some identifiers (variables) using equations.
-- You can print out the value of an identifier by typing C-c e and
-- typing the name of the identifier you wish to evaluate.  This will
-- compile the entire program, not just the line with the definition
-- you want to see.  Not all definitions are very interesting to print out -
-- by convention, we will use variables e1, e2, ... to denote values that
-- are `interesting' to print.

-- We'll start with some constants as well as their associated type.
-- There are two ways to associate a type with a value: a type declaration
-- and an expression type signature.  Here is an equation and a type
-- declaration:

e1 :: Int     -- This is a type declaration for the identifier e1
e1 = 5        -- This is an equation defining e1

-- You can evaluate the expression e1 and watch the system print `5'!  Wow!

-- Remember that C-c e is prompting for an expression.  Expressions like
-- e1 or 5 or 1+1 are all valid.  However, `e1 = 5' is a definition,
-- not an expression.  Trying to evaluate it will result in a syntax error.

-- The type declaration for e1 is not really necessary but we will try to
-- always provide type declarations for values to help document the program
-- and to ensure that the system infers the same type we do for an expression.
-- If you change the value for e1 to `True', the program will no longer
-- compile due to the type mismatch.

-- We will briefly mention expression type signatures: these are attached to 
-- expressions instead of identifiers.  Here are equivalent ways to do
-- the previous definition:

e2 = 5 :: Int
e3 = (2 :: Int) + (3 :: Int)

-- The :: has very low precedence in expressions and should usually be placed
-- in parenthesis.

-- Note that there are two completely separate languages: an expression
-- language for values and a type language for type signatures.  The type
-- language is used only in the type declarations previously described and
-- declarations of new types, described later.  Haskell uses a
-- uniform syntax so that values resemble their type signature as much as
-- possible.  However, you must always be aware of the difference between
-- type expressions and value expressions.

-- Here are some of the predefined types Haskell provides:
--    type           Value Syntax                Type Syntax
-- Small integers    <digits>                    Int
e4 :: Int
e4 = 12345
-- Characters        '<character>'               Char
e5 :: Char
e5 = 'a'
-- Boolean           True, False                 Bool
e6 :: Bool
e6 = True
-- Floating point    <digits.digits>             Float
e7 :: Float
e7 = 123.456
-- We will introduce these types now; there will be much more to say later.
-- Homogenous List   [<exp1>,<exp2>,...]         [<constituant type>]
e8 :: [Int]
e8 = [1,2,3]
-- Tuple             (<exp1>,<exp2>,...)         (<exp1-type>,<exp2-type>,...)
e9 :: (Char,Int)
e9 = ('b',4)
-- Functional        described later             domain type -> range type
succ :: Int -> Int  -- a function which takes an Int argument and returns Int
succ x = x + 1      -- test this by evaluating `succ 4'

-- Here's a few leftover examples from section 2:

e10 = succ (succ 3)  -- you could also evaluate `succ (succ 3)' directly
                     -- by entering the entire expression to the C-c e
 
-- If you want to evaluate something more complex than the `e' variables
-- defined here, it is better to enter a complex expression, such as
-- succ (succ 3), directly than to edit a new definition like e10 into
-- the program.  This is because any change to the program will require
-- recompilation of the entire page.  The expressions entered to C-c e are
-- compiled separately (and very quickly!).

-- Uncomment this next line to see a compile time type error.
-- e11 = 'a'+'b'
-- Don't worry about the error message - it will make more sense later.

-- Proceed to the next page using C-c C-f

-- Page 2   Section 2.1

-- Section 2.1   Polymorphic Types

module Test(Bool) where

-- The following line allows us to redefine functions in the standard
-- prelude.  Ignore this for now.

import Prelude hiding (length,head,tail,null)

-- start with some sample lists to use in test cases

list1 :: [Int]
list1 = [1,2,3]
list2 :: [Char]         -- This is the really a string
list2 = ['a','b','c']   -- This is the same as "abc"; evaluate list2 and see.
list3 :: [[a]]          -- The element type of the inner list is unknown
list3 = [[],[],[],[]]   -- so this list can't be printed
list4 :: [Int]
list4 = 1:2:3:4:[]      -- Exactly the same as [1,2,3,4]; print it and see.

-- This is the length function.  You can test it by evaluating expressions
-- such as `length list1'.  Function application is written by
-- simple juxtaposition: `f(x)' in other languages would be `f x' in Haskell.

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- Function application has the highest precedence, so 1 + length xs is
-- parsed as 1 + (length xs).  In general, you have to surround
-- non-atomic arguments to a function with parens.  This includes
-- arguments which are also function applications.  For example,
-- f g x is the function f applied to arguments g and x, similar to
-- f(g,x) in other languages.  However, f (g x) is f applied to (g x), or
-- f(g(x)), which means something quite different!  Be especially
-- careful with infix operators: f x+1 y-2 would be parsed as (f x)+(1 y)-2.
-- This is also true on the left of the `=': the parens around (x:xs) are
-- absolutely necessary.  length x:xs would be parsed as (length x):xs.

-- Also be careful with prefix negation, -.  The application `f -1' is
-- f-1, not f(-1).  Add parens around negative numbers to avoid this
-- problem.

-- Here are some other list functions:

head :: [a] -> a   -- returns the first element in a list (same as car in lisp)
head (x:xs) = x

tail :: [a] -> [a] -- removes the first element from a list (same as cdr)
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null (x:xs) = False

cons :: a -> [a] -> [a]
cons x xs = x:xs

nil :: [a]
nil = []

-- Length could be defined using these functions too.  This is
-- not good Haskell style but does illustrate these other list functions.
-- The if - then - else will be discussed later.  Haskell programmers feel
-- that the pattern matching style, as used in the previous version of
-- length, is more natural and readable.

length' :: [a] -> Int   -- Note that ' can be part of a name
length' x = if null x then 0 else 1 + length' (tail x)

-- A test case for length', cons, and nil

e1 = length' (cons 1 (cons 2 nil))

-- We haven't said anything about errors yet.  Each of the following
-- examples illustrates a potential runtime or compile time error.  The
-- compile time error is commented out so that other examples will compile;
-- you can uncomment them and see what happens.

-- e2 = cons True False   -- Why is this not possible in Haskell?
e3 = tail (tail ['a'])  -- What happens if you evaluate this?
e4 = []       -- This is especially mysterious!

-- This last example, e4, is something hard to explain but is often
-- encountered early by novices.  We haven't explained yet how the system
-- prints out the expressions you type in - this will wait until later.
-- However, the problem here is that e4 has the type [a].  The printer for
-- the list datatype is complaining that it needs to know a specific type
-- for the list elements even though the list has no elements!  This can
-- be avoided by giving e4 a type such as [Int].  (To further confuse you,
-- try giving e4 the type [Char] and see what happens.)

-- Page 3   Section 2.2

-- Section 2.2  User-Defined Types

module Test(Bool) where

-- The type Bool is already defined in the Prelude so there is no
-- need to define it here.

data Color = Red | Green | Blue | Indigo | Violet deriving Text
-- The `deriving Text' is necessary if you want to print a Color value.

-- You can now evaluate these expressions.
e1 :: Color
e1 = Red
e2 :: [Color]
e2 = [Red,Blue]

-- It is very important to keep the expression language and the type
-- language in Haskell separated.  The data declaration above defines
-- the type constructor Color.  This is a nullary constructor: it takes no
-- arguments.  Color is found ONLY in the type language - it can not be
-- part of an expression.  e1 = Color is meaningless.  (Actually, Color could
-- be both a data constructor and a type constructor but we'll ignore this
-- possibility for now).  On the other hand, Red, Blue, and so on are
-- (nullary) data constructors.  They can appear in expressions and
-- in patterns (described later).  The declaration e1 :: Blue would also
-- be meaningless.  Data constructors can be defined ONLY in a data
-- declaration.

-- In the next example, Point is a type constructor and Pt is a data
-- constructor.  Point takes one argument and Pt takes two.  A data constructor
-- like Pt is really just an ordinary function except that it can be used in
-- a pattern.  Type signatures can not be supplied directly for data
-- constructors; their typing is completely defined by the data declaration.
-- However, data constructors have a signature just like any variable:
-- Pt :: a -> a -> Point a   -- Not valid Haskell syntax
-- That is, Pt is a function which takes two arguments with the same
-- arbitrary type and returns a value containing the two argument values.

data Point a = Pt a a   deriving Text

e3 :: Point Float
e3 = Pt 2.0 3.0
e4 :: Point Char
e4 = Pt 'a' 'b'
e5 :: Point (Point Int)
e5 = Pt (Pt 1 2) (Pt 3 4)
-- e6 = Pt 'a' True         -- This is a typing error

-- The individual components of a point do not have names.
-- Let's jump ahead a little so that we can write functions using these
-- data types.  Data constructors (Red, Blue, ..., and Pt) can be used in
-- patterns.  When more than one equation is used to define a function,
-- pattern matching occurs top down.

-- A function to remove red from a list of colors.

removeRed :: [Color] -> [Color]
removeRed [] = []
removeRed (Red:cs) = removeRed cs
removeRed (c:cs) = c : removeRed cs  -- c cannot be Red at this point

e7 :: [Color]
e7 = removeRed [Blue,Red,Green,Red]

-- Pattern matching is capable of testing equality with a specific color.

-- All equations defining a function must share a common type.  A
-- definition such as:
-- foo Red = 1
-- foo (Pt x y) = x
-- would result in a type error since the argument to foo cannot be both a
-- Color and a Point.  Similarly, the right hand sides must also share a
-- common type; a definition such as
-- foo Red = Blue
-- foo Blue = Pt Red Red
-- would also result in a type error.

-- Here's a couple of functions defined on points.

dist :: Point Float -> Point Float -> Float
dist (Pt x1 y1) (Pt x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

midpoint :: Point Float -> Point Float -> Point Float
midpoint (Pt x1 y1) (Pt x2 y2) = Pt ((x1+x2)/2) ((y1+y2)/2)

p1 :: Point Float
p1 = Pt 1.0 1.0
p2 :: Point Float
p2 = Pt 2.0 2.0

e8 :: Float
e8 = dist p1 p2
e9 :: Point Float
e9 = midpoint p1 p2

-- The only way to take apart a point is to pattern match.
-- That is, the two values which constitute a point must be extracted
-- by matching a pattern containing the Pt data constructor.  Much
-- more will be said about pattern matching later.

-- Haskell prints values in the same syntax used in expressions.  Thus
-- Pt 1 2 would print as Pt 1 2 (of course, Pt 1 (1+1) would also print
-- as Pt 1 2).

-- Page 4  Section 2.3

-- Section 2.3  Recursive Types

module Test where

data Tree a = Leaf a | Branch (Tree a) (Tree a)    deriving Text

-- The following typings are implied by this declaration.  As before,
-- this is not valid Haskell syntax.
-- Leaf :: a -> Tree a
-- Branch :: Tree a -> Tree a -> Tree a

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right

-- The following trees can be used to test functions:

tree1 :: Tree Int
tree1 = Branch (Leaf 1) (Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 4))
tree2 :: Tree Int
tree2 = Branch (Branch (Leaf 3) (Leaf 1)) (Branch (Leaf 4) (Leaf 1))
tree3 :: Tree Int
tree3 = Branch tree1 tree2

-- Try evaluating `fringe tree1' and others.

-- Here's another tree function:

twist :: Tree a -> Tree a
twist (Branch left right) = Branch right left
twist x = x        -- This equation only applies to leaves

-- Here's a function which compares two trees to see if they have the
-- same shape.  Note the signature: the two trees need not contain the
-- same type of values.

sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf x) (Leaf y) = True
sameShape (Branch l1 r1) (Branch l2 r2) = sameShape l1 l2 && sameShape r1 r2
sameShape x y = False  -- One is a branch, the other is a leaf

-- The && function is a boolean AND function.

-- The entire pattern on the left hand side must match in order for the 
-- right hand side to be evaluated.  The first clause requires both 
-- arguments to be a leaf' otherwise the next equation is tested.  
-- The last clause will always match: the final x and y match both 
-- leaves and branches.

-- This compares a tree of integers to a tree of booleans.
e1 = sameShape tree1 (Branch (Leaf True) (Leaf False))

-- Page 5  Sections 2.4, 2.5, 2.6

-- Section 2.4  Type Synonyms

module Test(Bool) where

-- Since type synonyms are part of the type language only, it's hard to
-- write a program which shows what they do.  Essentially, they are like
-- macros for the type language.  They can be used interchangably with their
-- definition:

e1 :: String
e1 = "abc"
e2 :: [Char]   -- No different than String
e2 = e1

-- In the written tutorial the declaration of `Addr' is a data type
-- declaration, not a synonym declaration.  This shows that the data
-- type declaration as well as a signature can reference a synonym.

-- Section 2.5  Built-in Types

-- Tuples are an easy way of grouping a set of data values.  Here are
-- a few tuples.  Note the consistancy in notation between the values and
-- types.

e3 :: (Bool,Int)
e3 = (True,4)
e4 :: (Char,[Int],Char)
e4 = ('a',[1,2,3],'b')

-- Here's a function which returns the second component of a 3 tuple.
second :: (a,b,c) -> b
second (a,b,c) = b

-- Try out `second e3' and `second e4' - what happens?
-- Each different size of tuple is a completely distinct type.  There is
-- no general way to append two arbitrary tuples or randomly select the
-- i'th component of an arbitrary tuple.  Here's a function built using
-- 2-tuples to represent intervals.

-- Use a type synonym to represent homogenous 2 tuples
type Interval a = (a,a)

containsInterval :: Interval Int -> Interval Int -> Bool
containsInterval (xmin,xmax) (ymin,ymax) = xmin <= ymin && xmax >= ymax

p1 :: Interval Int
p1 = (2,3)
p2 :: Interval Int
p2 = (1,4)

e5 = containsInterval p1 p2
e6 = containsInterval p2 p1

-- Here's a type declaration for a type isomorphic to lists:

data List a = Nil | Cons a (List a) deriving Text

-- Except for the notation, this is completely equivalent to ordinary lists
-- in Haskell.

length' :: List a -> Int
length' Nil = 0
length' (Cons x y) = 1 + length' y

e7 = length' (Cons 'a' (Cons 'b' (Cons 'c' Nil)))

-- It's hard to demonstrate much about the `non-specialness' of built-in
-- types.  However, here is a brief summary:

-- Numbers and characters, such as 1, 2.2, or 'a', are the same as nullary
-- type constructors.

-- Lists have a special type constructor, [a] instead of List a, and
-- an odd looking data constructor, [].  The other data constructor, :, is
-- not `unusual', syntactically speaking.  The notation [x,y] is just
-- syntax for x:y:[] and "abc" for 'a' : 'b' : 'c' : [].

-- Tuples use a special syntax.  In a type expression, a 2 tuple containing
-- types a and be would be written (a,b) instead of using a prefix type
-- constructor such as Tuple2 a b.  This same notation is used to build
-- tuple values: (1,2) would construct a 2 tuple containing the values 1 and 2.


-- Page 6   Sections 2.5.1, 2.5.2

module Test(Bool) where

-- Section 2.5.1  List Comprehensions and Arithmetic Sequences

-- Warning: brackets in Haskell are used in three different types
-- of expressions: lists, as in [a,b,c], sequences (distinguished by
-- the ..), as in [1..2], and list comprehensions (distinguished by the
-- bar: |), as in [x+1 | x <- xs, x > 1].

-- Before list comprehensions, let's start out with sequences:

e1 :: [Int]
e1 = [1..10]   -- Step is 1
e2 :: [Int]
e2 = [1,3..10] -- Step is 3 - 1
e3 :: [Int]
e3 = [1,-1..-10]
e4 :: [Char]
e4 = ['a'..'z']   -- This works on chars too

-- We'll avoid infinite sequences like [1..] for now.  If you print one,
-- use C-c i to interrupt the Haskell program.

-- List comprehensions are very similar to nested loops.  They return a
-- list of values generated by the expression inside the loop.  The filter
-- expressions are similar to conditionals in the loop.

-- This function does nothing at all!  It just scans through a list and
-- copies it into a new one.

doNothing :: [a] -> [a]
doNothing l = [x | x <- l]

-- Adding a filter to the previous function allows only selected elements to
-- be generated.  This is similar to what is done in quicksort.

positives :: [Int] -> [Int]
positives l = [x | x <- l, x > 0]

e5 = positives [2,-4,5,6,-5,3]

-- Now the full quicksort function.

quicksort :: [Char] -> [Char]  -- Use Char just to be different!
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++
                   [x] ++
                   quicksort [y | y <- xs, y > x]

e6 = quicksort "Why use Haskell?"

-- Now for some nested loops.  Each generator, <-, adds another level of
-- nesting to the loop.  Note that the variable introduced by each generator
-- can be used in each following generator; all variables can be used in the
-- generated expression:

e7 :: [(Int,Int)]
e7 = [(x,y) | x <- [1..5], y <- [x..5]]

-- Now let's add some guards: (the /= function is `not equal')

e8 :: [(Int,Int)]
e8 = [(x,y) | x <- [1..7], x /= 5, y <- [x..8] , x*y /= 12]

-- This is the same as the loop: (going to a psuedo Algol notation)
-- for x := 1 to 7 do
--  if x <> 5 then
--   for y := x to 8 do
--    if x*y <> 12
--     generate (x,y)

-- Section 2.5.2  Strings

e9 = "hello" ++ " world"

-- Page 7    Sections 3, 3.1

module Test(Bool) where
import Prelude hiding (map)

-- Section 3   Functions

add :: Int -> Int -> Int
add x y = x+y

e1 :: Int
e1 = add 1 2

-- This Int -> Int is the latter part of the signature of add:
-- add :: Int -> (Int -> Int)

succ :: Int -> Int
succ = add 1

e2 :: Int
e2 = succ 3

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : (map f xs)

e3 :: [Int]
e3 = map (add 1) [1,2,3]
-- This next definition is the equivalent to e3
e4 :: [Int]
e4 = map succ [1,2,3]

-- Heres a more complex example.  Define flist to be a list of functions:
flist :: [Int -> Int]
flist = map add [1,2,3]
-- This returns a list of functions which add 1, 2, or 3 to their input.
-- Warning: Haskell should print flist as something like
--  [<<function>>,<<function>>,<<function>>]


-- Now, define a function which takes a function and returns its value
-- when applied to the constant 1:
applyTo1 :: (Int -> a) -> a
applyTo1 f = f 1

e5 :: [Int]
e5 = map applyTo1 flist  -- Apply each function in flist to 1

-- If you want to look at how the type inference works, figure out how
-- the signatures of map, applyTo1, and flist combine to yield [Int].

-- Section 3.1  Lambda Abstractions

-- The symbol \ is like `lambda' in lisp or scheme.

-- Anonymous functions are written as \ arg1 arg2 ... argn -> body
-- Instead of naming every function, you can code it inline with this
-- notation:

e6 = map (\f -> f 1) flist

-- Be careful with the syntax here.  \x->\y->x+y parses as
--  \ x ->\ y -> x + y.  The ->\ is all one token.  Use spaces!!

-- This is identical to e5 except that the applyTo1 function has no name.

-- Function arguments on the left of an = are the same as lambda on the
-- right:

add' = \x y -> x+y    -- identical to add
succ' = \x -> x+1     -- identical to succ

-- As with ordinary function, the parameters to anonymous functions
-- can be patterns:

e7 :: [Int]
e7 = map (\(x,y) -> x+y) [(1,2),(3,4),(5,6)]

-- Functions defined by more than one equation, like map, cannot
-- be converted to anonymous lambda functions quite as easily - a case
-- statement is also required.  This is discussed later.

-- Page 8   Sections 3.2, 3.2.1, 3.2.2

module Test(Bool) where

import Prelude hiding ((++),(.))

-- Section 3.2  Infix operators

-- Haskell has both identifiers, like x, and operators, like +.
-- These are just two different types of syntax for variables.
-- However, operators are by default used in infix notation.

-- Briefly, identifiers begin with a letter and may have numbers, _, and '
-- in them:  x, xyz123, x'', xYz'_12a.  The case of the first letter
-- distinguishes variables from data constructors (or type variables from
-- type constructors).  An operator is a string of symbols, where
-- :!#$%&*+./<=>?@\^| are all symbols.  If the first character is : then
-- the operator is a data constructor; otherwise it is an ordinary
-- variable operator.  The - and ~ characters may start a symbol but cannot
-- be used after the first character.  This allows a*-b to parse as
-- a * - b instead of a *- b.

-- Operators can be converted to identifiers by enclosing them in parens.
-- This is required in signature declarations.  Operators can be defined
-- as well as used in the infix style:

(++) :: [a] -> [a] -> [a]
[] ++ y = y
(x:xs) ++ y = x : (xs ++ y)

-- Table 2 (Page 54) of the report is invaluable for sorting out the
-- precedences of the many predefined infix operators.

e1 = "Foo" ++ "Bar"

-- This is the same function without operator syntax
appendList :: [a] -> [a] -> [a]
appendList [] y = y
appendList (x:xs) y = x : appendList xs y

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

add1 :: Int -> Int
add1 x = x+1

e2 = (add1 . add1) 3

-- Section 3.2.1  Sections

-- Sections are a way of creating unary functions from infix binary
-- functions.  When a parenthesized expression starts or ends in an
-- operator, it is a section.  Another definition of add1:

add1' :: Int -> Int
add1' = (+ 1)

e3 = add1' 4

-- Here are a few section examples:

e4 = map (++ "abc") ["x","y","z"]

e5 = map ("abc" ++) ["x","y","z"]


-- Section 3.2.2  Fixity Declarations

-- We'll avoid any demonstration of fixity declarations.  The Prelude
-- contains numerous examples.

-- Page 9  Sections 3.3, 3.4, 3.5
module Test(Bool) where

import Prelude hiding (take,zip)

-- Section 3.3  Functions are Non-strict

-- Observing lazy evaluation can present difficulties.  The essential
-- question is `does an expression get evaluated?'.  While in theory using a
-- non-terminating computation is the way evaluation issues are examined,
-- we need a more practical approach.  The `error' function serves as
-- a bottom value.  Evaluating this function prints an error message and
-- halts execution.

bot = error "Evaluating Bottom"

e1 :: Bool    -- This can be any type at all!
e1 = bot      -- evaluate this and see what happens.

const1 :: a -> Int
const1 x = 1

e2 :: Int
e2 = const1 bot  -- The bottom is not needed and will thus not be evaluated.

-- Section 3.4  "Infinite" Data Structures

-- Data structures are constructed lazily.  A constructor like : will not
-- evaluate its arguments until they are demanded.  All demands arise from
-- the need to print the result of the computation -- components not needed
-- to compute the printed result will not be evaluated.

list1 :: [Int]
list1 = (1:bot)

e3 = head list1    -- doesnt evaluate bot
e4 = tail list1    -- does evaluate bot

-- Some infinite data structures.  Don't print these!  If you do, you will
-- need to interrupt the system (C-c i) or kill the Haskell process.

ones :: [Int]
ones = 1 : ones

numsFrom :: Int -> [Int]
numsFrom n = n : numsFrom (n+1)

-- An alternate numsFrom using series notation:

numsFrom' :: Int -> [Int]
numsFrom' n = [n..]

squares :: [Int]
squares = map (^2) (numsFrom 0)

-- Before we start printing anything, we need a function to truncate these
-- infinite lists down to a more manageable size.  The `take' function
-- extracts the first k elements of a list:

take :: Int -> [a] -> [a]
take 0 x      = []                 -- two base cases: k = 0
take k []     = []                 -- or the list is empty
take k (x:xs) = x : take (k-1) xs

-- now some printable lists:

e5 :: [Int]
e5 = take 5 ones

e6 :: [Int]
e6 = take 5 (numsFrom 10)

e7 :: [Int]
e7 = take 5 (numsFrom' 0)

e8 :: [Int]
e8 = take 5 squares

-- zip is a function which turns two lists into a list of 2 tuples.  If
-- the lists are of differing sizes, the result is as long as the
-- shortest list.

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip xs ys = []   -- one of the lists is []

e9 :: [(Int,Int)]
e9 = zip [1,2,3] [4,5,6]

e10 :: [(Int,Int)]
e10 = zip [1,2,3] ones

fib :: [Int]
fib = 1 : 1 : [x+y | (x,y) <- zip fib (tail fib)]

e11 = take 5 fib

-- Let's do this without the list comprehension:

fib' :: [Int]
fib' = 1 : 1 : map (\(x,y) -> x+y) (zip fib (tail fib))

-- This could be written even more cleanly using a map function which
-- maps a binary function over two lists at once.  This is in the
-- Prelude and is called zipWith.

fib'' :: [Int]
fib'' = 1 : 1 : zipWith (+) fib (tail fib)

-- For more examples using infinite structures look in the demo files
-- that come with Yale Haskell.  Both the pascal program and the
-- primes program use infinite lists.

-- Section 3.5  The Error Function

-- Too late - we already used it!


-- Page 10   Sections 4, 4.1, 4.2

module Test(Bool) where

import Prelude hiding (take,(^))

-- Section 4  Case Expressions and Pattern Matching

-- Now for details of pattern matching.  We use [Int] instead of [a]
-- since the only value of type [a] is [].

contrived :: ([Int], Char, (Int, Float), String, Bool) -> Bool
contrived ([], 'b', (1, 2.0), "hi", True) = False
contrived x = True   -- add a second equation to avoid runtime errors

e1 :: Bool
e1 = contrived ([], 'b', (1, 2.0), "hi", True)
e2 :: Bool
e2 = contrived ([1], 'b', (1, 2.0), "hi", True)

-- Contrived just tests its input against a big constant.

-- Linearity in pattern matching implies that patterns can only compare
-- values with constants.  The following is not valid Haskell:

-- member x [] = False
-- member x (x:ys) = True      -- Invalid since x appears twice
-- member x (y:ys) = member x ys

f :: [a] -> [a]
f s@(x:xs) = x:s
f _ = []

e3 = f "abc"

-- Another use of _:

middle :: (a,b,c) -> b
middle (_,x,_) = x

e4 :: Char
e4 = middle (True, 'a', "123")

(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ (n+1) = x*(x^n)

e5 :: Int
e5 = 3^3
e6 :: Int
e6 = 4^(-2)  -- Notice the behavior of the + pattern on this one

-- Section 4.1  Pattern Matching Semantics

-- Here's an extended example to illustrate the left -> right, top -> bottom
-- semantics of pattern matching.

foo :: (Int,[Int],Int) -> Int
foo (1,[2],3)   = 1
foo (2,(3:_),3) = 2
foo (1,_,3)     = 3
foo _           = 4

bot = error "Bottom Evaluated"

e7 = foo (1,[],3)
e8 = foo (1,bot,3)
e9 = foo (1,1:bot,3)
e10 = foo (2,bot,2)
e11 = foo (3,bot,bot)

-- Now add some guards:

sign :: Int -> Int
sign x | x > 0  = 1
       | x == 0 = 0
       | x < 0  = -1

e12 = sign 3

-- The last guard is often `True' to catch all other cases.  The identifier
-- `otherwise' is defined as True for use in guards:

max' :: Int -> Int -> Int
max' x y | x > y      = x
         | otherwise  = y

-- Guards can refer to any variables bound by pattern matching.  When
-- no guard is true, pattern matching resumes at the next equation.  Guards
-- may also refer to values bound in an associated where declaration.


inOrder :: [Int] -> Bool
inOrder (x1:x2:xs) | x1 <= x2 = True
inOrder _                     = False

e13 = inOrder [1,2,3]
e14 = inOrder [2,1]

-- Section 4.2  An Example

take :: Int -> [a] -> [a]
take 0     _      = []
take _     []     = []
take (n+1) (x:xs) = x:take n xs

take' :: Int -> [a] -> [a]
take' _     []     = []
take' 0     _      = []
take' (n+1) (x:xs) = x:take' n xs

e15, e16, e17, e18 :: [Int]
e15 = take 0 bot
e16 = take' 0 bot
e17 = take bot []
e18 = take' bot []

-- Page 11    Sections 4.3, 4.4, 4.5, 4.6

module Test(Bool) where

-- import Prelude hiding (take,Request(..),Response(..)) -- Standard Haskell
import Prelude hiding (take)    -- Y2.0-b4 only

-- Section 4.3 Case Expressions

-- The function take using a case statement instead of multiple equations

take :: Int -> [a] -> [a]
take m ys = case (m,ys) of
             (0  ,_)    -> []
             (_  ,[])   -> []
             (n+1,x:xs) -> x : take n xs

-- The function take using if then else.  We can also eliminate the n+k
-- pattern just for fun.  The original version of take is much easier to read!

take' :: Int -> [a] -> [a]
take' m ys = if m == 0 then [] else
              if null ys then [] else
               if m > 0 then head ys : take (m-1) (tail ys)
                else error "m < 0"

-- Section 4.4  Lazy Patterns

-- Before the client-server example, here is a contrived example of lazy
-- patterns.  The first version will fail to pattern match whenever the
-- the first argument is [].  The second version will always pattern
-- match initially but x will fail if used when the list is [].

nonlazy :: [Int] -> Bool -> [Int]
nonlazy (x:xs) isNull  = if isNull then [] else [x]

e1 = nonlazy [1,2] False
e2 = nonlazy [] True
e3 = nonlazy [] False

-- This version will never fail the initial pattern match
lazy :: [Int] -> Bool -> [Int]
lazy ~(x:xs) isNull  = if isNull then [] else [x]

e4 = lazy [1,2] False
e5 = lazy [] True
e6 = lazy [] False

-- The server - client example is a little hard to demonstrate.  We'll avoid
-- the initial version which loops.  Here is the version with irrefutable
-- patterns.

type Response = Int
type Request = Int

client :: Request -> [Response] -> [Request]
client init ~(resp:resps) = init : client (next resp) resps

server :: [Request] -> [Response]
server (req : reqs) = process req : server reqs

-- Next maps the response from the previous request onto the next request
next :: Response -> Request 
next resp = resp

-- Process maps a request to a response
process :: Request -> Response
process req = req+1

requests :: [Request]
requests = client 0 responses

responses :: [Response]
responses = server requests

e7 = take 5 responses

-- The lists of requests and responses are infinite - there is no need to
-- check for [] in this program.  These lists correspond to streams in other
-- languages.

-- Here is fib again:

fib :: [Int]
fib@(1:tfib) = 1 : 1 : [ a+b | (a,b) <- zip fib tfib]

e8 = take 10 fib

-- Section 4.5  Lexical Scoping and Nested Forms

-- One thing that is important to note is that the order of the
-- definitions in a program, let expression, or where clauses is
-- completely arbitrary.  Definitions can be arranged 'top down'
-- or `bottom up' without changing the program.

e9 = let y = 2 :: Float
         f x = (x+y)/y
     in f 1 + f 2

f :: Int -> Int -> String
f x y | y > z  = "y > x^2"
      | y == z = "y = x^2"
      | y < z  = "y < x^2"
  where
    z = x*x

e10 = f 2 5
e11 = f 2 4

-- Section 4.6  Layout

-- There's nothing much to demonstrate here.  We have been using layout all
-- through the tutorial.  The main thing is to be careful line up the
-- first character of each definition.  For example, if you
-- change the indentation of the definition of f in e9 you will get a
-- parse error.

-- Page 12  Section 5
module Test(Bool) where

import Prelude hiding (elem)

-- Section 5  Type Classes

-- Names in the basic class structure of Haskell cannot be hidden (they are
-- in PreludeCore) so we have to modify the names used in the tutorial.

-- Here is a new Eq class:

class Eq' a where
  eq :: a -> a -> Bool

-- Now we can define elem using eq from above:

elem :: (Eq' a) => a -> [a] -> Bool
x `elem` [] = False
x `elem` (y:ys) = x `eq` y || x `elem` ys

-- Before this is of any use, we need to admit some types to Eq'

instance Eq' Int where
 x `eq` y = abs (x-y) < 3  -- Let's make this `nearly equal' just for fun

instance Eq' Float where
 x `eq` y = abs (x-y) < 0.1

list1 :: [Int]
list1 = [1,5,9,23]

list2 :: [Float]
list2 = [0.2,5.6,33,12.34]

e1 = 2 `elem` list1
e2 = 100 `elem` list1
e3 = 0.22 `elem` list2

-- Watch out!  Integers in Haskell are overloaded - without a type signature
-- to designate an integer as an Int, expressions like 3 `eq` 3 will be
-- ambiguous.  See 5.5.4 about this problem.

-- Now to add the tree type:

data Tree a = Leaf a | Branch (Tree a) (Tree a)   deriving Text

instance (Eq' a) => Eq' (Tree a) where
  (Leaf a)       `eq` (Leaf b)       = a `eq` b
  (Branch l1 r1) `eq` (Branch l2 r2) =  (l1 `eq` l2) && (r1 `eq` r2)
  _              `eq` _              = False

tree1,tree2 :: Tree Int
tree1 = Branch (Leaf 1) (Leaf 2)
tree2 = Branch (Leaf 2) (Leaf 1)

e4 = tree1 `eq` tree2

-- Now make a new class with Eq' as a super class:

class (Eq' a) => Ord' a where
 lt,le :: a -> a -> Bool          -- lt and le are operators in Ord'
 x `le` y = x `eq` y || x `lt` y  -- This is a default for le

-- The typing of lt & le is 
--  le,lt :: (Ord' a) => a -> a -> Bool
-- This is identical to
--  le,lt :: (Eq' a,Ord' a) => a -> a -> Bool

-- Make Int an instance of Ord
instance Ord' Int where
 x `lt` y = x < y+1

i :: Int  -- Avoid ambiguity
i = 3
e5 :: Bool
e5 = i `lt` i

-- Some constraints on instance declarations:
--   A program can never have more than one instance declaration for
--     a given combination of data type and class.
--   If a type is declared to be a member of a class, it must also be
--     declared in all superclasses of that class.
--   An instance declaration does not need to supply a method for every
--     operator in the class.  When a method is not supplied in an
--     instance declaration and no default is present in the class
--     declaration, a runtime error occurs if the method is invoked.
--   You must supply the correct context for an instance declaration --
--     this context is not inferred automatically.

-- Section 5.1  Equality and Ordered Classes
-- Section 5.2  Enumeration and Index Classes

-- No examples are provided for 5.1 or 5.2.  The standard Prelude contains
-- many instance declarations which illustrate the Eq, Ord, and Enum classes.

-- Page 13    Section 5.3

module Test(Bool) where

-- Section 5.3   Text and Binary Classes

-- This is the slow showTree.  The `show' function is part of the
-- Text class and works with all the built-in types.  The context `Text a'
-- arises from the call to show for leaf values.

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Text

showTree :: (Text a) => Tree a -> String
showTree (Leaf x)    = show x
showTree (Branch l r) = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

tree1 :: Tree Int
tree1 = Branch (Leaf 1) (Branch (Leaf 3) (Leaf 6))

e1 = showTree tree1

-- Now the improved showTree; shows is already defined for all
-- built in types.

showsTree  :: Text a => Tree a -> String -> String
showsTree (Leaf x) s = shows x s
showsTree (Branch l r) s = '<' : showsTree l ('|' : showsTree r ('>' : s))

e2 = showsTree tree1 ""

-- The final polished version.  ShowS is predefined in the Prelude so we
-- don't need it here. 


showsTree'  :: Text a => Tree a -> ShowS
showsTree' (Leaf x) = shows x
showsTree' (Branch l r) = ('<' :) . showsTree' l . ('|' :) .
                          showsTree' r . ('>' :)

e3 = showsTree' tree1 ""


-- Page 14    This page break is just to keep recompilation from getting too
--            long.  The compiler takes a little longer to compile this
--            page than other pages.

module Test(Bool) where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Text

-- Now for the reading function.  Again, ReadS is predefined and reads works
-- for all built-in types.  The generators in the list comprehensions are
-- patterns: p <- l binds pattern p to successive elements of l which match
-- p.  Elements not matching p are skipped.

readsTree :: (Text a) => ReadS (Tree a)
readsTree ('<':s)  = [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                        (r, '>':u) <- readsTree t ]
readsTree s        = [(Leaf x,t)      | (x,t) <- reads s]

e4 :: [(Int,String)]
e4 = reads "5 golden rings"

e5 :: [(Tree Int,String)]
e5 = readsTree "<1|<2|3>>"
e6 :: [(Tree Int,String)]
e6 = readsTree "<1|2"
e7 :: [(Tree Int,String)]
e7 = readsTree "<1|<<2|3>|<4|5>>> junk at end"

-- Before we do the next readTree, let's play with the lex function.

e8 :: [(String,String)]
e8 = lex "foo bar bletch"

-- Here's a function to completely lex a string.  This does not handle
-- lexical ambiguity - lex would return more than one possible lexeme
-- when an ambiguity is encountered and the patterns used here would not
-- match.

lexAll :: String -> [String]
lexAll s = case lex s of
            [("",_)] -> []  -- lex returns an empty token if none is found
            [(token,rest)] -> token : lexAll rest

e9 = lexAll "lexAll :: String -> [String]"
e10 = lexAll "<1|<a|3>>"

-- Finally, the `hard core' reader.  This is not sensitive to
-- white space as were the previous versions.


readsTree' :: (Text a) => ReadS (Tree a)
readsTree' s = [(Branch l r, x) | ("<", t) <- lex s,
				  (l, u)   <- readsTree' t,
                                  ("|", v) <- lex u,
                                  (r, w)   <- readsTree' v,
				  (">", x) <- lex w ]
                ++
                [(Leaf x, t)    | (x, t) <- reads s]

-- When testing this program, you must make sure the input conforms to
-- Haskell lexical syntax.  If you remove spaces between | and < or
-- > and > they will lex as a single token.

e11 :: [(Tree Int,String)]
e11 = readsTree' "<1 | <2 | 3> >"
e12 :: [(Tree Bool,String)]
e12 = readsTree' "<True|False>"

-- Finally, here is a simple version of read for trees only:

read' :: (Text a) => String -> (Tree a)
read' s = case (readsTree' s) of
           [(tree,"")] -> tree   -- Only one parse, no junk at end
           []          -> error "Couldn't parse tree"
           [_]         -> error "Crud after the tree"  -- unread chars at end
           _           -> error "Ambiguous parse of tree"

e13 :: Tree Int
e13 = read' "foo"
e14 :: Tree Int
e14 = read' "< 1 | < 2 | 3 > >"
e15 :: Tree Int
e15 = read' "3 xxx"

-- Page 15  Section 5.4

module Test(Bool) where

-- Section 5.4  Derived Instances

-- We have actually been using the derived Text instances all along for
-- printing out trees and other structures we have defined.  The code
-- in the tutorial for the Eq and Ord instance of Tree is created
-- implicitly by the deriving clause so there is no need to write it
-- here.

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq,Ord,Text)

-- Now we can fire up both Eq and Ord functions for trees:

tree1, tree2, tree3, tree4 :: Tree Int
tree1 = Branch (Leaf 1) (Leaf 3)
tree2 = Branch (Leaf 1) (Leaf 5)
tree3 = Leaf 4
tree4 = Branch (Branch (Leaf 4) (Leaf 3)) (Leaf 5)

e1 = tree1 == tree1
e2 = tree1 == tree2
e3 = tree1 < tree2

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++
                   [x] ++
                   quicksort [y | y <- xs, y > x]

e4 = quicksort [tree1,tree2,tree3,tree4]

-- Now for Enum: 

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday |
           Friday | Saturday     deriving (Text,Eq,Ord,Enum)

e5 = quicksort [Monday,Saturday,Friday,Sunday]
e6 = [Wednesday .. Friday]
e7 = [Monday, Wednesday ..]
e8 = [Saturday, Friday ..]


-- Page 16  Sections 5.5, 5.5.1, 5.5.2, 5.5.3

module Test(Bool) where

-- Section 5.5  Numbers
-- Section 5.5.1  Numeric Class Structure
-- Section 5.5.2  Constructed Numbers

-- Here's a brief summary of Haskell numeric classes.

-- Class Num
--   Most general numeric class.  Has addition, subtraction, multiplication.
--   Integers can be coerced to any instance of Num with fromInteger.
--   All integer constants are in this class.
-- Instances: Int, Integer, Float, Double, Ratio a, Complex a

-- Class Real
--   This class contains ordered numbers which can be converted to
--   rationals.
-- Instances: Int, Integer, Float, Double, Ratio a

-- Class Integral
--   This class deals with integer division.  All values in Integral can
--   be mapped onto Integer.
-- Instances: Int, Integer

-- Class Fractional
--   These are numbers which can be divided.  Any rational number can
--   be converted to a fractional.  Floating point constants are in
--   this class: 1.2 would be 12/10.
-- Instances: Float, Double, Ratio a

-- Class Floating
--   This class contains all the standard floating point functions such
--   as sqrt and sin.
-- Instances: Float, Double, Complex a

-- Class RealFrac
--   These values can be rounded to integers and approximated by rationals.
-- Instances: Float, Double, Ratio a

-- Class RealFloat
--   These are floating point numbers constructed from a fixed precision
--   mantissa and exponent.
-- Instances: Float, Double

-- There are only a few sensible combinations of the constructed numerics
-- with built-in types:
--  Ratio Integer (same as Rational): arbitrary precision rationals
--  Ratio Int: limited precision rationals
--  Complex Float: complex numbers with standard precision components
--  Complex Double: complex numbers with double precision components


-- The following function works for arbitrary numerics:

fact :: (Num a) => a -> a
fact 0 = 1
fact n = n*(fact (n-1))

-- Note the behavior when applied to different types of numbers:

e1 :: Int
e1 = fact 6
e2 :: Int
e2 = fact 20   -- Yale Haskell may not handle overflow gracefully!
e3 :: Integer
e3 = fact 20
e4 :: Rational
e4 = fact 6
e5 :: Float
e5 = fact 6
e6 :: Complex Float
e6 = fact 6

-- Be careful: values like `fact 1.5' will loop!

-- As a practical matter, Int operations are much faster than Integer
-- operations.  Also, overloaded functions can be much slower than non-
-- overloaded functions.  Giving a function like fact a precise typing:

-- fact :: Int -> Int

-- will yield much faster code.

-- In general, numeric expressions work as expected.  Literals are
-- a little tricky - they are coerced to the appropriate value.  A
-- constant like 1 can be used as ANY numeric type.

e7 :: Float
e7 = sqrt 2
e8 :: Rational
e8 = ((4%5) * (1%2)) / (3%4)
e9 :: Rational
e9 = 2.2 * (3%11) - 1
e10 :: Complex Float
e10 = (2 * (3:+3)) / (1.1:+2.0 - 1)
e11 :: Complex Float
e11 = sqrt (-1)
e12 :: Integer
e12 = numerator (4%2)
e13 :: Complex Float
e13 = conjugate (4:+5.2)

-- A function using pattern matching on complex numbers:

mag :: (RealFloat a) => Complex a -> a
mag (a:+b) = sqrt (a^2 + b^2)

e14 :: Float
e14 = mag (1:+1)

-- Section 5.5.3  Numeric Coercions and Overloaded Literals

-- The Haskell type system does NOT implicitly coerce values between
-- the different numeric types!  Although overloaded constants are 
-- coerced when the overloading is resolved, no implicit coercion goes
-- on when values of different types are mixed.  For example:

f :: Float
f = 1.1
i1 :: Int
i1 = 1
i2 :: Integer
i2 = 2

-- All of these expressions would result in a type error (try them!):

-- g = i1 + f
-- h = i1 + i2
-- i3 :: Int
-- i3 = i2

-- Appropriate coercions must be introduced by the user to allow
-- the mixing of types in arithmetic expressions.

e15 :: Float
e15 = f + fromIntegral i1
e16 :: Integer
e16 = fromIntegral i1 + i2
e17 :: Int
e17 = i1 + fromInteger i2  -- fromIntegral would have worked too.

-- Page 17  Section 5.5.4
module Test(Bool) where

-- Section 5.5.4  Default Numeric Types

-- Ambiguous contexts arise frequently in numeric expressions.  When an
-- expression which produces a value with a general type, such as
-- `1' (same as `fromInteger 1'; the type is (Num a) => a), with
-- another expression which `consumes' the type, such as `show' or
-- `toInteger', ambiguity arises.  This ambiguity can be resolved
-- using expression type signatures, but this gets tedious fast!  
-- Assigning a type to the top level of an ambiguous expression does
-- not help: the ambiguity does not propagate to the top level.

e1 :: String -- This type does not influence the type of the argument to show
e1 = show 1  -- Does this mean to show an Int or a Float or ...
e2 :: String
e2 = show (1 :: Float)
e3 :: String
e3 = show (1 :: Complex Float)

-- The reason the first example works is that ambiguous numeric types are
-- resolved using defaults.  The defaults in effect here are Int and
-- Double.  Since Int `fits' in the expression for e1, Int is used.
-- When Int is not valid (due to other context constraints), Double
-- will be tried.

-- This function defaults the type of the 2's to be Int

rms :: (Floating a) => a -> a -> a
rms x y = sqrt ((x^2 + y^2) * 0.5)

-- The C-c e evaluation used to the Haskell system also makes use of
-- defaulting.  When you type an expression, the system creates a
-- simple program to print the value of the expression using a function
-- like show.  If no type signature for the printed expression is given,
-- defaulting may occur.

-- One of the reasons for adding type signatures throughout these examples
-- is to avoid unexpected defaulting.  Many of the top level signatures are
-- required to avoid ambiguity.

-- Defaulting can lead to overflow problems when values exceed Int limits.
-- Evaluate a very large integer without a type signature to observe this
-- (unfortunately this may cause a core dump or other unpleasantness).

-- Notice that defaulting applies only to numeric classes.  The
--   show (read "xyz")                       -- Try this if you want!
-- example uses only class Text so no defaulting occurs.

-- Ambiguity also arises with polymorphic types.  As discussed previously,
-- expressions like [] have a similar problem.

-- e4 = []   -- Won't work since [] has type [a] and `a' is not known.

-- Note the difference: even though the lists have no components, the type
-- of component makes a difference in printing.

e5 = ([] :: [Int]) 
e6 = ([] :: [Char])

-- Page 18   Sections 6, 6.1, 6.2

-- Section 6  Modules

module Tree ( Tree(Leaf,Branch), fringe ) where
--            Tree(..) would work also

data Tree a = Leaf a | Branch (Tree a) (Tree a)   deriving Text

fringe :: Tree a -> [a]
fringe (Leaf x)             = [x]
fringe (Branch left right)  = fringe left ++ fringe right

-- The Emacs interface to Haskell performs evaluation within the
-- module containing the cursor.  To evaluate e1 you must place the
-- cursor in module Main.

module Main (Tree) where
import Tree ( Tree(Leaf, Branch), fringe)
-- import Tree      -- this would be the same thing
e1 :: [Int]
e1 = fringe (Branch (Leaf 1) (Leaf 2))

-- This interactive Haskell environment can evaluate expressions in
-- any module.  The use of module Main is optional.

-- Section 6.1  Original Names and Renaming

module Renamed where
import Tree ( Tree(Leaf,Branch), fringe)
    renaming (Leaf to Root, Branch to Twig)

e2 :: Tree Int
e2 = Twig (Root 1) (Root 2)  -- Printing always uses the original names

-- Section 6.2  Interfaces and Implementations

-- Yale Haskell allows separate compilation of modules using
-- unit files.  These are described in the user's guide.


-- Page 19  Sections 6.3, 6.4

-- Section 6.3  Abstract Data Types

-- Since TreeADT does not import Tree it can use the name Tree without
-- any conflict.  Each module has its own separate namespace.

module TreeADT (Tree, leaf, branch, cell, left,
               right, isLeaf) where

data Tree a = Leaf a | Branch (Tree a) (Tree a)    deriving Text

leaf = Leaf
branch = Branch
cell (Leaf a) = a
left (Branch l r) = l
right (Branch l r) = r
isLeaf (Leaf _) = True
isLeaf _        = False

module Test where
import TreeADT

-- Since the constructors for type Tree are hidden, pattern matching
-- cannot be used.

fringe :: Tree a -> [a]
fringe x = if isLeaf x then [cell x]
                       else fringe (left x) ++ fringe (right x)

e1 :: [Int]
e1 = fringe (branch (branch (leaf 3) (leaf 2)) (leaf 1))

-- Section 6.4


-- Page 20  Sections 7, 7.1, 7.2, 7.3

-- Section 7  Typing Pitfalls

-- Section 7.1  Let-Bound Polymorphism

module Test(e2) where

-- f g = (g 'a',g [])    -- This won't typecheck.

-- Section 7.2  Overloaded Numerals

-- Overloaded numerics were covered previously - here is one more example.
-- sum is a prelude function which sums the elements of a list.

average :: (Fractional a) => [a] -> a
average xs   = sum xs / fromIntegral (length xs)

e1 :: Float   -- Note that e1 would default to Double instead of Int - 
              -- this is due to the Fractional context.
e1 = average [1,2,3]

-- Section 7.3  The Monomorphism Restriction

-- The monomorphism restriction is usually encountered when functions
-- are defined without parameters.  If you remove the signature for sum'
-- the monomorphism restriction will apply.
-- This will generate an error if either:
--   sum' is added to the module export list at the start of this section
--   both sumInt and sumFloat remain in the program.
-- If sum' is not exported and all uses of sum' have the same overloading,
-- there is no type error.

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0         -- foldl reduces a list with a binary function
                           -- 0 is the initial value.

sumInt :: Int
sumInt = sum' [1,2,3]

sumFloat :: Float
sumFloat = sum' [1,2,3]

-- If you use overloaded constants you also may encounter monomorphism:

x :: Num a => a
x = 1    -- The type of x is Num a => a
y :: Int
y = x            -- Uses x as an Int
z :: Integer
z = x          -- Uses x as an Integer.  A monomorphism will occur of the
               -- signature for x is removed.
                 -- comments to see an error.

-- Finally, if a value is exported it must not be overloaded unless bound
-- by a function binding.  e2 is the only value exported.

e2 :: Int  -- Remove this to get an error.  Without this line e1 will
           -- be overloaded.
e2 = 1

-- To prevent annoying error messages about exported monomorphic variables,
-- most modules in this tutorial do not implicitly export everything - they
-- only export a single value, Bool, which was chosen to keep the export
-- list non-empty (a syntactic restriction!).  In Haskell systems without
-- the evaluator used here, a module which does not export any names would
-- be useless.

-- module Test where  -- this would export everything in the module
-- module Test(Bool)  -- exports only Bool
-- module Test()      -- this is what we really want to do but is not valid.

-- Page 21  Sections 8, 8.1

module Test(Bool) where

-- Section 8  Input/Output
-- Section 8.1  Introduction to Continuations

-- Simplify f here to be 1/x.

data Maybe a  = Ok a | Oops String deriving Text

f :: Float -> Maybe Float
f x = if x == 0 then Oops "Divide by 0" else Ok (1/x)

-- g is a `safe' call to x.  The call to error could be replaced by
-- some explicit value like Oops msg -> 0.

g x = case f x of
        Ok y -> y
        Oops msg -> error msg
   
e1 = f 0
e2 = g 0
e3 = g 1

-- Here is the same example using continuations:

f' :: Float -> (String -> Float) -> Float
f' x c = if x == 0 then c "Divide by 0"
                   else 1/x

g' x = f' x error   -- calls error on divide by 0
g'' x = f' x (\s -> 0) -- returns 0 on divide by 0

e4 = g' 0
e5 = g'' 0

-- Page 22  Sections 8.2, 8.3

module Test where

-- Section 8.2  Continuation Based I/O

-- We will skip the program fragments at the start of this section and
-- move directly to the writeFile / readFile example.

-- Before we can use Haskell I/O, we need to introduce a new Emacs command:
-- C-c r.  This command runs a dialogue instead of printing a value.
-- (Actually C-c e creates a dialogue on the fly and runs it in the same
-- manner as C-c r).  As with C-c e you are prompted for an expression.
-- In this case, the expression must be of type Dialogue and it is
-- executed by the I/O system.  We use d1,d2,... for dialogues to be
-- executed by C-c r.

-- We make the file name a parameter to allow for easier testing.
-- Don't expect much error handling in exit.

s1 = "This is a test of Haskell"

main file = writeFile file s1 exit (
            readFile  file    exit (\s2 ->
            appendChan stdout (if s1==s2 then "contents match"
                                         else "something intervened!") exit
            done))	

d1,d2 :: Dialogue
d1 = main "/tmp/ReadMe"
d2 = main "/dev/null" -- this will read back as the empty string

-- A simple IO program using $ for readability: ($ is defined in the Prelude)

d3 = appendChan "stdout" "Type something: " exit $
     readChan "stdin" exit $ \s2 ->
     appendChan "stdout" ("You typed " ++ head (lines s2)) exit $
     done

-- This program suffers from a strictness problem.  Strictness deals
-- with when things get evaluated.  In this program, the input is not
-- needed until after the "You typed " is printed.  Fixing this would
-- require some operation to look at the string before the final 
-- appendChan.  Here is one possible fix:
 
d4 = appendChan "stdout" "Type something: " exit $
     readChan "stdin" exit $ \s2 ->
     let str = head (lines s2) in
      if str == str then  -- This evaluates str
       appendChan "stdout" ("You typed " ++ head (lines s2)) exit $
       done
      else done


-- Section 8.3  Terminal I/O

-- Since this programming environment runs under Emacs, the issue of
-- echoing does not really apply.  However, the synchronization between
-- input and output can be seen in the following example.  Since the input
-- comes a line at a time, the X's come in groups between input lines.
-- The cursor will move into the haskell dialogue buffer when the program
-- requests input.  Use a ^D to stop the program (^Q^D actually).  [Warning:
-- some brain damaged lisps stop not only the Haskell program but also
-- the entire compiler on ^D]

d5 = readChan stdin exit processInput where
      processInput s = loop 1 s
      loop n [] = done
      loop n (x:xs) | n == 10  = appendChan stdout "X" exit (loop 1 xs)
                    | True     = loop (n+1) xs
 
-- For more examples using the I/O system look in the demo programs
-- that come with haskell (in $HASKELL/progs/demo) and the report.

-- Page 23  Sections 9, 9.1, 9.2

module Test(Bool) where

-- Section 9  Arrays
-- Section 9.1  Index Types

-- Arrays are built on the class Ix.  Here are some quick examples of Ix:

e1 :: [Int]
e1 = range (0,4)
e2 :: Int
e2 = index (0,4) 2
low,high :: (Int,Int)
low = (1,1)
high = (3,4)
e3 = range (low,high)
e4 = index (low,high) (3,2)
e5 = inRange (low,high) (4,3)

-- Section 9.2  Array Creation

squares :: Array Int Int
squares = array (1,100) [i := i*i | i <- [1..100]]

-- We can also parameterize this a little:

squares' :: Int -> Array Int Int
squares' n = array (1,n) [i := i*i | i <- [1..n]]

e6 :: Int
e6 = squares!6
e7 :: (Int,Int)
e7 = bounds squares
e8 :: Array Int Int
e8 = squares' 10

-- Here is a function which corresponds to `take' for lists.  It takes
-- an arbitrary slice out of an array.

atake :: (Ix a) => Array a b -> (a,a) -> Array a b
atake a (l,u) | inRange (bounds a) l && inRange (bounds a) u =
                   array (l,u) [i := a!i | i <- range (l,u)]
              | otherwise = error "Subarray out of range"

e9 = atake squares (4,8)

mkArray :: Ix a => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [i := f i | i <- range bnds]

e10 :: Array Int Int
e10 = mkArray (\i -> i*i) (1,10)

fibs :: Int -> Array Int Int
fibs n = a where
            a = array (0,n) ([0 := 1, 1 := 1] ++
                             [i := a!(i-1) + a!(i-2) | i <- [2..n]])

e11 = atake (fibs 50) (3,10)

wavefront :: Int -> Array (Int,Int) Int
wavefront n = a where
                a = array ((1,1),(n,n))
                     ([(1,j) := 1 | j <- [1..n]] ++
                      [(i,1) := 1 | i <- [2..n]] ++
                      [(i,j) := a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j)
                                  | i <- [2..n], j <- [2..n]])

wave = wavefront 20
e12 = atake wave ((1,1),(3,3))
e13 = atake wave ((3,3),(5,5))

-- Here are some errors in array operations:

e14 :: Int
e14 = wave ! (0,0)  -- Out of bounds
arr1 :: Array Int Int
arr1 = array (1,10) [1 := 1] -- No value provided for 2..10
e15,e16 :: Int
e15 = arr1 ! 1  -- works OK
e16 = arr1 ! 2  -- undefined by array

-- Page 24  Sections 9.3, 9.4

module Test(Bool) where

-- Section 9.3  Accumulation

hist :: (Ix a, Integral b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [i := 1 | i <- is, inRange bnds i]

e1 :: Array Char Int
e1 = hist ('a','z') "This counts the frequencies of each lowercase letter"

decades :: (RealFrac a) => a -> a -> [a] -> Array Int Int
decades a b = hist (0,9) . map decade
                where
                  decade x = floor ((x-a) * s)
                  s = 10 / (b - a)

test1 :: [Float]
test1 = map sin [0..100]  -- take the sine of the 0 - 100
e2 = decades 0 1 test1

-- Section 9.4  Incremental Updates

swapRows :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows i i' a = a // ([(i,j) := a!(i',j) | j <- [jLo..jHi]] ++
			[(i',j) := a!(i,j) | j <- [jLo..jHi]])
               where ((iLo,jLo),(iHi,jHi)) = bounds a

arr1 :: Array (Int,Int) (Int,Int)
arr1 = array ((1,1),(5,5)) [(i,j) := (i,j) | (i,j) <- range ((1,1),(5,5))]

e3 = swapRows 2 3 arr1

-- Printing the arrays in more readable form makes the results easier
-- to view.

-- This is a printer for 2d arrays

aprint a width = shows (bounds a) . showChar '\n' . showRows lx ly where
  showRows r c | r > ux = showChar '\n'
  showRows r c | c > uy = showChar '\n' . showRows (r+1) ly
  showRows r c = showElt (a!(r,c)) . showRows r (c+1)
  showElt e = showString (take width (show e ++ repeat ' ')) . showChar ' '
  ((lx,ly),(ux,uy)) = bounds a

showArray a w = appendChan stdout (aprint a w "") abort done

d1 = showArray e3 6

swapRows' :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows' i i' a = a // [assoc | j <- [jLo..jHi],
                                 assoc <- [(i,j) := a!(i',j),
	  				   (i',j) := a!(i,j)]]
               where ((iLo,jLo),(iHi,jHi)) = bounds a

d2 = showArray (swapRows' 1 5 arr1) 6

-- Page 25  Section 9.5

module Test(Bool) where

-- Section 9.5  An example: Matrix Multiplication

aprint a width = shows (bounds a) . showChar '\n' . showRows lx ly where
  showRows r c | r > ux = showChar '\n'
  showRows r c | c > uy = showChar '\n' . showRows (r+1) ly
  showRows r c = showElt (a!(r,c)) . showRows r (c+1)
  showElt e = showString (take width (show e ++ repeat ' ')) . showChar ' '
  ((lx,ly),(ux,uy)) = bounds a

showArray a w = appendChan stdout (aprint a w "") abort done

matMult :: (Ix a, Ix b, Ix c, Num d) =>
              Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y =
  array resultBounds
        [(i,j) := sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)]
                  | i <- range (li,ui),
                    j <- range (lj',uj')]
 where
    ((li,lj),(ui,uj)) = bounds x
    ((li',lj'),(ui',uj')) = bounds y
    resultBounds
      | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
      | otherwise             = error "matMult: incompatible bounds"

mat1,mat2,mat3,mat4 :: Array (Int,Int) Int
mat1 = array ((0,0),(1,1)) [(0,0) := 1,(0,1) := 0,(1,0) := 0,(1,1) := 1]
mat2 = array ((0,0),(1,1)) [(0,0) := 1,(0,1) := 1,(1,0) := 1,(1,1) := 1]
mat3 = array ((0,0),(1,1)) [(0,0) := 1,(0,1) := 2,(1,0) := 3,(1,1) := 4]
mat4 = array ((0,0),(1,2)) [(0,0) := 1,(0,1) := 2,(0,2) := 3,
			    (1,0) := 4,(1,1) := 5,(1,2) := 6]

d1 = showArray (matMult mat1 mat2) 4
d2 = showArray (matMult mat2 mat3) 4
d3 = showArray (matMult mat1 mat4) 4
d4 = showArray (matMult mat4 mat1) 4

matMult' :: (Ix a, Ix b, Ix c, Num d) =>
              Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult' x y =
  accumArray (+) 0 ((li,lj'),(ui,uj'))
        [(i,j) := x!(i,k) * y!(k,j)
                  | i <- range (li,ui),
                    j <- range (lj',uj'),
                    k <- range (lj,uj)]

 where
    ((li,lj),(ui,uj)) = bounds x
    ((li',lj'),(ui',uj')) = bounds y
    resultBounds
       | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
       | otherwise             = error "matMult: incompatible bounds"

d5 = showArray (matMult mat1 mat2) 4
d6 = showArray (matMult mat2 mat3) 4

genMatMul :: (Ix a, Ix b, Ix c) =>
              ([f] -> g) -> (d -> e -> f) ->
              Array (a,b) d -> Array (b,c) e -> Array (a,c) g
genMatMul f g x y =
  array ((li,lj'),(ui,uj'))
        [(i,j) := f [(x!(i,k)) `g` (y!(k,j)) | k <- range (lj,uj)]
                  | i <- range (li,ui),
                    j <- range (lj',uj')]
 where
    ((li,lj),(ui,uj)) = bounds x
    ((li',lj'),(ui',uj')) = bounds y
    resultBounds
         | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
         | otherwise             = error "matMult: incompatible bounds"

d7 = showArray (genMatMul maximum (-) mat2 mat1) 4
d8 = showArray (genMatMul and (==) mat1 mat2) 6
d9 = showArray (genMatMul and (==) mat1 mat1) 6

-- Page 26     More about Haskell

This is the end of the tutorial.  If you wish to see more examples of
Haskell programming, Yale Haskell comes with a set of demo programs.
These can be found in $HASKELL/progs/demo.  Once you have mastered the
tutorial, both the report and the user manual for Yale Haskell should
be understandable.  Many examples of Haskell programming can be found in
the Prelude.  The directory $HASKELL/progs/prelude contains the sources
for the Prelude.

We appreciate any comments you have on this tutorial.  Send any comments
to haskell-requests@cs.yale.edu.

   The Yale Haskell Group
