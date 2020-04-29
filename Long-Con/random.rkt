#lang racket
(provide (all-defined-out))

;; Randomly generate an expression
(define (random-expr)
  (contract-random-generate
   (flat-rec-contract e
                      (integer-in #f #f)
                      (list/c 'add1 e)
                      (list/c 'sub1 e)
                      (list/c 'if (list/c 'zero? e) e e)
                      (list/c '- e)
                      (list/c 'abs e)
                      (cons/c 'cond
                              (list*of (list/c (list/c 'zero? e) e)
                                       (list/c (list/c 'else e)))))))
