module Pretty(text, separate, nest, pretty, (~.), (^.), IText(..), Context(..)) where
infixr 8 ~.
infixr 8 ^.

type IText   = Context -> [String]
type Context = (Bool,Int,Int,Int)

text :: String -> IText
text s (v,w,m,m') = [s]

(~.) :: IText -> IText -> IText
(~.) d1 d2 (v,w,m,m') =
	let t = d1 (False,w,m,m')
            tn = last t
	    indent = length tn
	    sig = if length t == 1
		  then m' + indent
		  else length (dropWhile (==' ') tn)
	    (l:ls) = d2 (False,w-indent,m,sig)
	in  init t ++
	    [tn ++ l] ++
	    map (space indent++) ls

space :: Int -> String
space n = [' ' | i<-[1..n]]

(^.) :: IText -> IText -> IText
(^.) d1 d2 (v,w,m,m') = d1 (True,w,m,m') ++ d2 (True,w,m,0)

separate :: [IText] -> IText
separate [] _ = [""]
separate ds (v,w,m,m') = 
	let hor = foldr1 (\d1 d2 -> d1 ~. text " " ~. d2) ds
	    ver = foldr1 (^.) ds
	    t = hor (v,w,m,m')
	in  if fits 1 t && fits (w `min` m-m') (head t)
	    then t
	    else ver (v,w,m,m')

fits n xs = length xs <= n `max` 0 --null (drop n xs)

nest :: Int -> IText -> IText
nest n d (v,w,m,m') = 
	if v then
	    map (space n++) (d (v,w-n,m,if m'==0 then 0 else m'+n)) 
	else 
	    d (v,w,m,m')

pretty :: Int->Int->IText->String
pretty w m d = concat (map (++"\n") (d (False,w,m,0)))
