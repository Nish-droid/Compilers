#lang racket
(provide lex-port lex-string)

;; NOTE: you don't need to modify (or understand) this file.
;; But there's nothing complicated going on.  It's just like the 330 lexer,
;; using regular expressions to tokenize input.

;; String -> [Listof Token]
(define (lex-string s)
  (lex-port (open-input-string s)))

;; InputPort -> [Listof Token]
(define (lex-port p)
  (unless (regexp-try-match #px"^[[:space:]]*#lang racket($|[[:space:]])" p)
    (error "must start with #lang racket"))
  (let loop ()
    (cond
      [(regexp-try-match "^$" p) (list 'eof)]
      [(regexp-try-match #px"^[[:space:]]+" p) (loop)]
      [else
       (cons
        (cond         
          [(regexp-try-match (keywords kws) p)
           => (compose string->symbol bytes->string/utf-8 first)]
          [(regexp-try-match (string-append "^" (regexp-quote "(")) p) 'lparen]
          [(regexp-try-match (string-append "^" (regexp-quote ")")) p) 'rparen]
          [(regexp-try-match (string-append "^" (regexp-quote "[")) p) 'lsquare]
          [(regexp-try-match (string-append "^" (regexp-quote "]")) p) 'rsquare]        
          [(regexp-try-match "^[-]?[0-9]+" p)
           => (compose string->number bytes->string/utf-8 first)]          
          [else (error "lexing error")])
        (loop))])))

(define kws '("add1" "sub1" "else" "cond" "zero?" "abs" "-" "if"))

(define delim  
  (string-append "$|"
                 (regexp-quote " ")  "|"
                 (regexp-quote "\n") "|"
                 (regexp-quote "\t") "|"
                 (regexp-quote "(")  "|"
                 (regexp-quote ")")  "|"
                 (regexp-quote "[")  "|"
                 (regexp-quote "]")))

(define (keywords ws)
  (string-append
   "^("
   (apply string-append (add-between (map regexp-quote ws) "|"))
   ")(?=" delim ")"))


(module+ test
  (require rackunit)
  (check-equal? (lex-string "#lang racket") '(eof))
  (check-equal? (lex-string "#lang racket 1") '(1 eof))
  (check-equal? (lex-string "#lang racket 123") '(123 eof))
  (check-equal? (lex-string "#lang racket (") '(lparen eof))
  (check-equal? (lex-string "#lang racket (") '(lparen eof))
  (check-equal? (lex-string "#lang racket [") '(lsquare eof))
  (check-equal? (lex-string "#lang racket )") '(rparen eof))
  (check-equal? (lex-string "#lang racket ]") '(rsquare eof))
  (check-equal? (lex-string "#lang racket add1") '(add1 eof))
  (check-equal? (lex-string "#lang racket sub1") '(sub1 eof))
  (check-equal? (lex-string "#lang racket else") '(else eof))
  (check-equal? (lex-string "#lang racket cond") '(cond eof))
  (check-equal? (lex-string "#lang racket zero?") '(zero? eof))
  (check-equal? (lex-string "#lang racket (cond [(zero? 0) 1] [else 2])")
                '(lparen cond lsquare lparen zero? 0 rparen 1 rsquare
                         lsquare else 2 rsquare rparen eof)))
