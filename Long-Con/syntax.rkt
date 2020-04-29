#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(if (zero? ,x) ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]

    ;;TODO..
    [`(abs ,x) (expr? x)]
    [`(- ,x) (expr? x)]
    [`(cond ,@x)
     (cond? x)
    ]
    
    [`(else ,x) (expr? x)]
    [`(cond [else ,x]) (expr? x)]

    [`([(zero? ,x0) ,x1] ,x2)
     (and (expr? x0)
          (expr? x1)
          (expr? x2))
    ]
    ;;TODO..
    [_ #f])
)

(define (cond? cs)
  (match cs
    [`([else ,e]) (expr? e)]
    [`([(zero? ,e) ,e2] ,@cs2)
     (and
      (expr? e)
      (expr? e2)
      (cond? cs2))]
    
   )
)
