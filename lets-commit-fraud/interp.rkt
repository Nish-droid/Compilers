#lang racket
(provide (all-defined-out))

;; type Value =
;; | Integer
;; | Boolean
;; | Character

;; type Answer = Value | 'err

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (let ((a (interp-env e r)))
       (interp-prim p a))]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]    
    [(? symbol? x)
     (lookup r x)]
    [`(let ,(list `(,xs ,es) ...) ,e)
     (match (interp-envs es r)
       ['err 'err]
       [vs
        (if (any-err? vs)
          'err
          (interp-env e (append (zip xs vs) r)))])]
    [`(let* ,(list bs ...) ,e)
     (match (map-accum-env bs r)
       ['err 'err]
       [new-r
        (interp-env e new-r)])]
    [(list 'cond cs ... `(else ,en))
     (interp-cond-env cs en r)]))

(define (any-err? xs)
  (match xs
    ['() #f]
    ['err #t]
    [(cons y ys)
     (if (eq? 'err y) #t (any-err? ys))]))

;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-envs es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-envs es r))])]))

;; (Listof (Symbol Expr)) REnv -> REnv | 'err 
(define (map-accum-env bs r)
  (match bs
    ['() r]
    [(cons (list b e) bss)
     (match (interp-env e r)
       ['err 'err]
       [v (map-accum-env bss (cons (list b v) r))])]))

;; (Listof (List Expr Expr)) Expr REnv -> Answer
(define (interp-cond-env cs en r)
  (match cs
    ['() (interp-env en r)]
    [(cons `(,eq ,ea) cs)
     (match (interp-env eq r)
       ['err 'err]
       [v
        (if v
            (interp-env ea r)
            (interp-cond-env cs en r))])]))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero? abs - char? boolean? integer? integer->char char->integer))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)
      (char? x)))

;; Prim Answer -> Answer
(define (interp-prim p a)
  (match (list p a)
    [(list p 'err) 'err]
    [(list '- (? integer? i0)) (- i0)]    
    [(list 'abs (? integer? i0)) (abs i0)]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'char? v0) (char? v0)]
    [(list 'integer? v0) (integer? v0)]
    [(list 'boolean? v0) (boolean? v0)]
    [(list 'integer->char (? codepoint? i0)) (integer->char i0)]
    [(list 'char->integer (? char? c)) (char->integer c)]
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))

;; Any -> Boolean
(define (codepoint? x)
  (and (integer? x)
       (<= 0 x #x10FFFF)
       (not (<= #xD800 x #xDFFF))))

;; (Listof A) (Listof B) -> (Listof (List A B))
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]))
