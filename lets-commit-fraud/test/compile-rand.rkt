#lang racket
(require (only-in "../interp.rkt" interp)
         (only-in "../compile.rkt" compile)
         "../asm/interp.rkt" 
         "../random-exprs.rkt" 
         rackunit)

(define (check-compiler e)
  (check-equal? (interp e)
                (asm-interp (compile e))              
                e))

(for ([e exprs])
  (check-compiler e))

