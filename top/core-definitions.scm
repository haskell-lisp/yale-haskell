;;; This file defines core symbols - those in PreludeCore and
;;; other Prelude symbols used in compilation.

;;; This part is constructed from the export table of PreludeCore
;;; by 'top/prelude-core-syms' and has been pasted in here.


(DEFINE *haskell-prelude-vars*
  '((CLASSES "Num"
             "Integral"
             "Eq"
             "Text"
             "Fractional"
             "RealFloat"
             "RealFrac"
             "Enum"
             "Ix"
             "Floating"
             "Ord"
             "Real"
             "Binary")
    (METHODS "fromInteger"
             "signum"
             "abs"
             "negate"
             "*"
             "-"
             "+"
             "toInteger"
             "odd"
             "even"
             "divMod"
             "quotRem"
             "mod"
             "div"
             "rem"
             "quot"
             "/="
             "=="
             "showList"
             "readList"
             "showsPrec"
             "readsPrec"
             "fromRational"
             "recip"
             "/"
             "scaleFloat"
             "significand"
             "exponent"
             "encodeFloat"
             "decodeFloat"
             "floatRange"
             "floatDigits"
             "floatRadix"
             "floor"
             "ceiling"
             "round"
             "truncate"
             "properFraction"
             "enumFromThenTo"
             "enumFromTo"
             "enumFromThen"
             "enumFrom"
             "inRange"
             "index"
             "range"
             "atanh"
             "acosh"
             "asinh"
             "tanh"
             "cosh"
             "sinh"
             "atan"
             "acos"
             "asin"
             "tan"
             "cos"
             "sin"
             "logBase"
             "**"
             "sqrt"
             "log"
             "exp"
             "pi"
             "min"
             "max"
             ">"
             ">="
             "<="
             "<"
             "toRational"
             "showBin"
             "readBin")
    (TYPES "Char"
           "Complex"
           "Integer"
           "Double"
           "Bin"
           "Array"
           "Float"
           "Bool"
           "Int"
           "Assoc"
           "Ratio"
           "SystemState"
           "IOResult")
    (CONSTRUCTORS ":+" "True" "False" ":=" ":")
    (SYNONYMS "ShowS" "ReadS" "String" "Rational" "IO")
    (VALUES)))

;;; Non PreludeCore stuff

;;; This table defines all symbols in the core used internally by the
;;; compiler.

(define *haskell-noncore-vars* '(
  (types 
     "List"
     "Arrow"
     "Request"
     "Response"
     "UnitType"
     "TupleDicts")
  (constructors 
     "MkFloat"
     "MkDouble"
     "MkChar"
     "MkInteger"
     "MkInt"
     "Nil"
     "UnitConstructor")
  (values
    "&&"  "||"  "primPlusInt"
    "++" "take" "drop" "." "showChar" "shows" "showString"
    "showParen" "lex" "readParen" "reads"
    "primShowBinInt" "primReadBinSmallInt"
    "error"
    "primIntegerToInt" "primIntToInteger"
    "primRationalToFloat" "primRationalToDouble"
    "primNegInt" "primNegInteger" "primNegFloat" "primNegDouble" 
    "foldr" "build" "inlineFoldr" "inlineBuild" 
    "primAppend" "primStringEq"
    "dictSel" "tupleEqDict" "tupleOrdDict" "tupleIxDict"
    "tupleTextDict" "tupleBinaryDict")))





