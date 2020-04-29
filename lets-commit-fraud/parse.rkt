#lang racket
(provide parse)

; type Token =
; | Integer
; | Char
; | Boolean
; | ‘(variable ,Variable)
; | ‘(keyword ,Keyword)
; | ‘(prim ,Prim)
; | 'lparen    ;; (
; | 'rparen    ;; )
; | 'lsquare   ;; [
; | 'rsquare   ;; ]
; | 'eof       ;; end of file

; type Variable = Symbol (other than 'let, 'cond, etc.)

; type Keyword =
; | 'let
; | 'cond
; | 'else
; | 'if

; type Prim =
; | 'add1
; | 'sub1
; | 'zero?
; | 'abs
; | '-
; | 'integer->char
; | 'char->integer
; | 'char?
; | 'boolean?
; | 'integer?

;; (Listof Token) -> Expr
(define (parse lot)
  ; Select whichever parser you want to start from
  (parse-imperative lot)
  ;
  ;(parse-functional lot)
)

;; Any -> Boolean
(define (prim? p)
  (match p
    [`(prim ,_) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imperative approach

(define *input* (box '()))

(define (parse-imperative lot)
  (set-box! *input* lot)
  (let ((e (parse-expr!))
        (_ (match-tok! 'eof)))
    e))

;; -> Token
(define (look-ahead)
  (match (unbox *input*)
    ['() (error "no look-ahead")]
    [(cons x _) x]))

;; -> Token
(define (look-ahead2)
  (match (unbox *input*)
    ['() (error "no look-ahead")]
    [(cons _ '()) (error "no look-ahead 2")]
    [(cons _ (cons x _)) x]))

;; Token -> Token
(define (match-tok! t)
  (match (unbox *input*)
    ['() (error "no token")]
    [(cons x xs)
     (if (equal? t x)
         (begin (set-box! *input* xs) x)
         (error "parse error"))]))

;; -> Expr
(define (parse-expr!)
  (match (look-ahead)
    [(? integer? i) (match-tok! i)]
    [(? boolean? b) (match-tok! b)]
    [(? char? c) (match-tok! c)]
    [`(variable ,k) (parse-var2!)]

    ['lparen
     (let ((lp (match-tok! 'lparen))
           (e  (parse-compound!))
           (rp (match-tok! 'rparen)))
       e)]

    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (e  (parse-compound!))
           (rp (match-tok! 'rsquare)))
       e)]
  )
)

;; -> Expr
(define (parse-compound!)
  (match (look-ahead)
     [(? prim? p)
     (let ((p (match-tok! p))
           (e (parse-expr!)))
       (match p
         [`(prim ,p) `(,p ,e)]))]

    ['(keyword if)
     (let ((if (match-tok! '(keyword if)))
           (q (parse-question!))
           (e1 (parse-expr!))
           (e2 (parse-expr!)))
       `(if ,q ,e1 ,e2))]
       
    ['(keyword cond)
     (let ((c (match-tok! '(keyword cond)))
           (cs (parse-clauses!))
           (el (parse-else!)))
       `(cond ,@cs ,el))]
    
    ['(keyword let)
      (let ((c (match-tok! '(keyword let)))
            (xs (parse-bindings!))
            (e1 (parse-expr!)))  
        
        `(let (,@xs) (,@e1))
      )
    ]

    ['(keyword let*)
      (let ((c (match-tok! '(keyword let*)))
            (xs (parse-bindings!))
            (e1 (parse-expr!)))  

        `(let* (,@xs) (,@e1))
      )
    ]
  )
)

(define (parse-bindings!)
  (match (look-ahead)
    ['lparen 
      (let ((lp (match-tok! 'lparen))
            (xs (parse-binding!))
            (rp (match-tok! 'rparen)))

          `(,@xs)
      )
    ]

    ['lsquare
      (let ((lp (match-tok! 'lsquare))
            (xs (parse-binding!))
            (rp (match-tok! 'rsquare)))
        
          `(,@xs)
      )
    ]
  )
)

(define (parse-binding!)
  (match (look-ahead)
    [(or 'rsquare 'rparen)
      '()
    ]
    ['lparen
      (let ((lp (match-tok! 'lparen))
            (vs (parse-var2!))
            (e1 (parse-expr!))            
            (rp (match-tok! 'rparen))
            (at (parse-binding!))) 

         (cons (list vs e1) at)
      )
    ]
    ['lsquare
      (let ((lp (match-tok! 'lsquare))
            (vs (parse-var2!))
            (e1 (parse-expr!))
            (at (parse-binding!))
            (rp (match-tok! 'rsquare))) 

         (cons (list vs e1) at)
      )
    ]
    ['rparen
      '()
    ]
    ['rsquare
      '()
    ]
  )
)

(define (parse-var2!)
  (match (look-ahead)
    [`(variable ,k) 
      (let ((vr (match-tok! `(variable ,k))))
        k
      )
    ]
  )
)

;; -> (Listof (List (List 'zero? Expr) Expr))
(define (parse-clauses!)
  (match (look-ahead)
    [(or 'lparen 'lsquare)
     (match (look-ahead2)
       ['(keyword else) '()]
       [_
        (let ((c (parse-clause!))
              (cs (parse-clauses!)))
          (cons c cs))])]
    [_ '()]))

;; -> (List (List 'zero? Expr) Expr)
(define (parse-clause!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (q (parse-expr!))                    ;(q (parse-question!))
           (a (parse-expr!))
           (rp (match-tok! 'rparen)))
       (list q a))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (q (parse-expr!))                     ;(q (parse-question!))
           (a (parse-expr!))
           (rp (match-tok! 'rsquare)))
       (list q a))]))

;; -> (Listof (List 'else Expr))
(define (parse-else!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (el (match-tok! '(keyword else)))
           (e  (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(else ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (el (match-tok! '(keyword else)))
           (e  (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(else ,e))]))

;; -> (List 'zero? Expr)
(define (parse-question!)
  (match (look-ahead)
    [(or 'lparen 'lsquare)
      (match (look-ahead2)
        ['(prim zero?)
          (parse-zero?!)          
        ]
        ['(prim integer?)
          (parse-integer?!)
        ]
        ['(prim boolean?)
          (parse-boolean?!)
        ]
        ['(prim char?)
          (parse-char?!)
        ]
        ['(prim add1)
          (parse-add1!)
        ]
        ['(prim sub1)
          (parse-sub1!)
        ]
        ['(prim abs)
          (parse-abs!)
        ]
        ['(prim -)
          (parse--!)
        ]
        ['(prim integer->char?)
          (parse-integer->char?!)
        ]
        ['(prim char->integer?)
          (parse-char->integer?!)
        ]
        [_ (parse-expr!)]
      )
    ]
  )
)

(define (parse-zero?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim zero?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(zero? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim zero?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(zero? ,e))]
  )
)
    
(define (parse-integer?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim integer?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(integer? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim integer?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(integer? ,e))]
  )
)

(define (parse-boolean?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim boolean?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(boolean? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim boolean?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(boolean? ,e))]
  )
)

(define (parse-char?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim char?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(char? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim char?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(char? ,e))]
  )
)

(define (parse-add1!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim add1)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(add1 ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim add1)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(add1 ,e))]
  )
)

(define (parse-sub1!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim sub1)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(sub1 ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim sub1)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(sub1 ,e))]
  )
)

(define (parse-abs!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim abs)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(abs ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim abs)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(abs ,e))]
  )
)

(define (parse--!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim -)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(- ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim -)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(- ,e))]
  )
)

(define (parse-integer->char?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim integer->char?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(integer->char? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim integer->char?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(integer->char? ,e))]
  )
)

(define (parse-char->integer?!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim char->integer?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(char->integer? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim char->integer?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(char->integer? ,e))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)
  (require "lex.rkt")
  ;; String -> Expr
  (define (p s)
    (parse (lex-string (string-append "#lang racket " s))))

   (check-equal? (p "7") 7)
  (check-equal? (p "(add1 7)") '(add1 7))
  (check-equal? (p "(sub1 7)") '(sub1 7))
  (check-equal? (p "[add1 7]") '(add1 7))
  (check-equal? (p "[sub1 7]") '(sub1 7))
  (check-equal? (p "(abs 7)") '(abs 7))
  (check-equal? (p "[abs 7]") '(abs 7))  (check-equal? (p "(- 7)") '(- 7))
  (check-equal? (p "[- 7]") '(- 7))
  (check-equal? (p "(cond [else 1])") '(cond [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [else 1])")
                '(cond [(zero? 0) 2] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] (else 1))")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(if (zero? 9) 1 2)")
                '(if (zero? 9) 1 2))
   (check-equal? (p "(cond [else 5])") '(cond [else 5]))
   (check-equal? (p "(cond [(zero? 1) 2] [else 3])") 
                     '(cond [(zero? 1) 2] [else 3]))
   (check-equal? (p "(cond [(zero? 0) 2] [else 3])") 
                    '(cond [(zero? 0) 2] [else 3]))
   (check-equal? (p "(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])") 
                    '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3]))
   
   (check-equal? (p "(cond [(integer? 3) 3] [else 4])")
                    '(cond [(integer? 3) 3] [else 4]))
   (check-equal? (p "(cond [(add1 -1) 3] [else 4])")
                    '(cond [(add1 -1) 3] [else 4]))

  (check-equal? (p "(cond [#t 2] [else 3])") 
  '(cond [#t 2] [else 3]))
  (check-equal? (p "(cond [#f 2] [else 3])") 
  '(cond [#f 2] [else 3]))
  (check-equal? (p "(cond [1 2] [else 3])") 
  '(cond [1 2] [else 3]))
  (check-equal? (p "(cond [#f 2] [#t 4] [else 3])") 
  '(cond [#f 2] [#t 4] [else 3]))
  (check-equal? (p "(cond [#t 2] [#f 4] [else 3])") 
  '(cond [#t 2] [#f 4] [else 3]))
  (check-equal? (p "(cond [#t 2] [#f (add1 #f)] [else 3])") 
  '(cond [#t 2] [#f (add1 #f)] [else 3]))
  
  (check-equal? (p "(zero? #t)") '(zero? #t))
 (check-equal? (p "(zero? #f)") '(zero? #f))
 (check-equal? (p "(add1 #f)") '(add1 #f))
 (check-equal? (p "(if (add1 #f) 1 2)") '(if (add1 #f) 1 2))
 (check-equal? (p "(abs #f)") '(abs #f))
 (check-equal? (p "(cond [(add1 #f) 1] [else 2])") '(cond [(add1 #f) 1] [else 2]))
 ;(check-equal? (p "(integer->char (sub1 #xD800))") '(integer->char (sub1 #xD800)))

 (check-equal? (p "(let () 7)") '(let () 7))
 (check-equal? (p "(let ((x 7)) x)") '(let ((x 7)) x))
 (check-equal? (p "(let ((x 7)) 2)") '(let ((x 7)) 2))
 (check-equal? (p "(let ((x 7)) (add1 x))") '(let ((x 7)) (add1 x)))
 (check-equal? (p "(let ((x (add1 7))) x)") '(let ((x (add1 7))) x))
 (check-equal? (p "(let ((x 7)) (let ((y 2)) x))") '(let ((x 7)) (let ((y 2)) x)))
 (check-equal? (p "(let ((x 7)) (let ((x 2)) x))") '(let ((x 7)) (let ((x 2)) x)))
 (check-equal? (p "(let ((x 7)) (let ((x (add1 x))) x))") '(let ((x 7)) (let ((x (add1 x))) x)))
 (check-equal? (p "(let ((x (add1 #f))) 0)") '(let ((x (add1 #f))) 0))
 (check-equal? (p "(let () 7)") '(let () 7))
 (check-equal? (p "(let ((x 7) (y 8)) 2)") '(let ((x 7) (y 8)) 2))
 (check-equal? (p "(let ((x 7) (y 8)) (add1 x))") '(let ((x 7) (y 8)) (add1 x)))
 (check-equal? (p "(let ((x 7) (y 8)) (add1 y))") '(let ((x 7) (y 8)) (add1 y)))
 (check-equal? (p "(let ((x (add1 7)) (y 0)) y)") '(let ((x (add1 7)) (y 0)) y))
 (check-equal? (p "(let ((x 7) (z 9)) (let ((y 2)) x))") '(let ((x 7) (z 9)) (let ((y 2)) x)))
 (check-equal? (p "(let ((x 7) (z 9)) (let ((x 2)) x))") '(let ((x 7) (z 9)) (let ((x 2)) x)))
 (check-equal? (p "(let ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x))") '(let ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x)))
 (check-equal? (p "(let ((x (add1 #f)) (z 9)) x)") '(let ((x (add1 #f)) (z 9)) x))
 (check-equal? (p "(let* () 7)") '(let* () 7))
 (check-equal? (p "(let* ((x 7) (y 8)) 2)") '(let* ((x 7) (y 8)) 2))
 (check-equal? (p "(let* ((x 7) (y 8)) (add1 x))") '(let* ((x 7) (y 8)) (add1 x)))
 (check-equal? (p "(let* ((x 7) (y x)) (add1 x))") '(let* ((x 7) (y x)) (add1 x)))
 (check-equal? (p "(let* ((x 8) (y x)) (add1 y))") '(let* ((x 8) (y x)) (add1 y)))
 (check-equal? (p "(let* ((x (add1 7)) (y 0)) y)") '(let* ((x (add1 7)) (y 0)) y))
 (check-equal? (p "(let* ((x 7) (z 9)) (let ((y 2)) x))") '(let* ((x 7) (z 9)) (let ((y 2)) x)))
 (check-equal? (p "(let* ((x 7) (z 9)) (let ((x 2)) x))") '(let* ((x 7) (z 9)) (let ((x 2)) x)))
 (check-equal? (p "(let* ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x))") '(let* ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x)))
 (check-equal? (p "(let* ((x (add1 #f)) (z 9)) x)") '(let* ((x (add1 #f)) (z 9)) x))
;  (check-equal? (p "(char? #\a)") '(char? #\a))
;  (check-equal? (p "(integer? #\a)") '(integer? #\a))
;  (check-equal? (p '(boolean? #\a)) #f)
  (check-equal? (p "(char? 4)") '(char? 4))
  (check-equal? (p "(integer? 4)") '(integer? 4))
  (check-equal? (p "(boolean? 4)") '(boolean? 4))
  (check-equal? (p "(char? #f)") '(char? #f))
  (check-equal? (p "(integer? #f)") '(integer? #f))
  (check-equal? (p "(boolean? #t)") '(boolean? #t))
  ;(check-equal? (p '(char->integer #\a)) 97)
  (check-equal? (p "(integer->char 97)") '(integer->char 97))
;  (check-equal? (p "(integer->char #\a)") '(integer->char #\a))
  ;(check-equal? (p "(integer->char (sub1 #xD800))") '(integer->char (sub1 #xD800)))
  #;...)
