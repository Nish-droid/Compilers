#lang racket
(provide parse)

; type Token =
; | Integer
; | 'add1
; | 'sub1
; | 'zero?
; | 'if
; | 'cond
; | 'else
; | 'abs
; | '-
; | 'lparen    ;; (
; | 'rparen    ;; )
; | 'lsquare   ;; [
; | 'rsquare   ;; ]
; | 'eof       ;; end of file

; [Listof Token] -> Expr
(define (parse lot)
  (match (parse-expr lot)
    [(cons '(eof) e) e] ;;Checking on singleton list of eof and expr
    [(cons pon _) (error (~a pon))]
    [_ (error "parse error")]))
 
; [Listof Token] -> (Pairof [Listof Token] Expr)
(define (parse-expr lot)
  (match lot

    [(cons (? integer? i) rest)
     (match rest
       ['(eof) (cons rest i)]
       [_ (cons rest `(,i))]
       )
     ]
    
    [(cons 'lparen rest)
     (parse-compound rest)
     ]
    
    [(cons 'lsquare rest)
     (parse-compound rest)
     ]

    [(cons 'rparen rest)
     (parse-compound rest)
     ]
    
    [(cons 'rsquare rest)
     (parse-compound rest)
     ]

    [(cons 'add1 rest)
     (parse-compound rest)
     ]

    [eof
     eof
     ]
   )
 )

;;(token list, expr)
(define (parse-compound lot)
  (match lot
    [(cons 'add1 rest)

     ;(match rest
      ; [(cons 'lparen rst)
       ; (cons 'lparen (parse-compound rst))
        ;]
      ;)
        
     (let ((p1 (parse-expr rest)))
       (match p1
         [(cons `(rparen ,@tl) exp)
          (cons tl (cons 'add1 exp))
          ]     
         [(cons `(rsquare ,@tl) exp)
          (cons tl (cons 'add1 exp))
          ]     
        )
      )
     ]


    [(cons 'sub1 rest)

     (let ((p1 (parse-expr rest)))
       (match p1
         [(cons `(rparen ,@tl) exp)
          (cons tl (cons 'sub1 exp))
          ]
         [(cons `(rsquare ,@tl) exp)
          (cons tl (cons 'sub1 exp))
          ]
        )
      )
     ]

    [(cons 'abs rest)

     (let ((p1 (parse-expr rest)))
       (match p1
         [(cons `(rparen ,@tl) exp)
          (cons tl (cons 'abs exp))
          ]
         [(cons `(rsquare ,@tl) exp)
          (cons tl (cons 'abs exp))
          ]
        )
      )
     ]

     [(cons '- rest)

     (let ((p1 (parse-expr rest)))
       (match p1
         [(cons `(rparen ,@tl) exp)
          (cons tl (cons '- exp))
          ]
         [(cons `(rsquare ,@tl) exp)
          (cons tl (cons '- exp))
          ]
        )
      )
     ]

   )
 )




(module+ test
  (require rackunit)
  (require "lex.rkt")
  ;; String -> Expr
  (define (p s)
    (parse (lex-string (string-append "#lang racket " s))))

  (check-equal? (p "7") 7)
  (check-equal? (p "(add1 7)") '(add1 7))
;  (check-equal? (p "(add1 (add1 7))") '(add1 (add1 7)))
  (check-equal? (p "(sub1 7)") '(sub1 7))
  (check-equal? (p "[add1 7]") '(add1 7))
  (check-equal? (p "[sub1 7]") '(sub1 7))
  (check-equal? (p "(abs 7)") '(abs 7))
  (check-equal? (p "[abs 7]") '(abs 7))
  (check-equal? (p "[abs 7]") '(abs 7))  
  (check-equal? (p "(- 7)") '(- 7))
  (check-equal? (p "[- 7]") '(- 7))
  (check-equal? (p "(cond [else 1])") '(cond [else 1]))
 ; (check-equal? (p "(cond [(zero? 0) 2] [else 1])")
  ;              '(cond [(zero? 0) 2] [else 1]))
 ; (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])")
  ;              '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  ;(check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] (else 1))")
   ;             '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  ;(check-equal? (p "(if (zero? 9) 1 2)")
   ;             '(if (zero? 9) 1 2))
  ;; TODO: add more tests
  #;...)
