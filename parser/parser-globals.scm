;;; Global vars used in the parser

(define *current-line* '())  ;  current line the scanner is on
(define *current-col* '())   ;  current col; valid at start of line &
                             ;  after where,let,of

;;; Lexer

(define *lex-literate?* '#f)
(define *start-line* 0)
(define *start-col* 0)
(define *on-new-line?* '#t)
(define *save-col?* '#f)
(define *port* '())
(define *tokens* '())
(define *char* 0)
(define *peek-char* 0)
(define *at-eof/p?* 0)
(define *at-eof?* 0)
(define *on-new-line? '#f)

;;; Parser

(define *token-stream* '())  ;  remaining tokens to be parsed
(define *token* '())         ;  current token type
(define *token-args* '())    ;  current token arguments
(define *layout-stack* '())  ;  columns at which layout is being done
