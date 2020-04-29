#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? symbol? s) #t]

    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
   
    [`(,(? prim?) ,x) (expr? x)]
   
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
   
   [`(let ,(list `(,xs ,es) ...) ,e)
      (and 
           (andmap expr? es)
           (andmap expr? xs)
           (ndmap xs)
           (expr? e)
      )      
    ]
  [`(let* ,(list `(,xs ,es) ...) ,e)
      (and 
           (andmap expr? es)
           (andmap expr? xs)
           (ndmap xs)
           (expr? e)
      )      
    ]
    
    ;; TODO
    ;; ...
    [_ #f]
  )
)

;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? symbol? s) #t]

    [`(if ,x ,y ,z)
     (and (closed? x)
          (closed? y)
          (closed? z))]
   
    [`(,(? prim?) ,x) (closed? x)]
   
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (closed? z))]
   
    [`(let ,(list `(,xs ,es) ...) ,e)
      (and 
           (andmap expr? es)
           (andmap expr? xs)
           (ndmap xs)
           (closed? e)
      )      
    ]
    
    [`(let* ,(list `(,xs ,es) ...) ,e)      
      (check-let* xs es e 1)
    ]

    ;; TODO
    [_ #f]
  )
)

(define (returns lst n)
  (list-tail (reverse lst) n)
)

(define (is-in-list2 list value)
  (if (symbol? value)
    (cond
      [(empty? list) false]
      [(equal? (first list) value) true]
      [else (is-in-list2 (rest list) value)]
    )
    true
  ))

(define (check-let* xs es e num)
 (if (= num (length xs))
  #t
  (match es 
    ['() #t]
    [(cons f rest)
      (if (is-in-list2 (returns xs num) f) (check-let* xs rest e (add1 num)) #f) 
    ]
  )))

(define (ndmap mp) (if (check-duplicates mp) #f #t))

;; Any -> Boolean
;; Is x a primitive?
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      char? integer? boolean? zero?))))

(define (is-in-list list value)
 (cond
  [(empty? list) false]
  [(= (first list) value) true]
  [else (is-in-list (rest list) value)]))
