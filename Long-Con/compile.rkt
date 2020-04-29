#lang racket
(provide (all-defined-out))

;; This assignment should be completed individually.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I pledge on my honor that I have not given or received any
;; unauthorized assistance on this assignment.
;;
;; Name: Nischay M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e)
    ret))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(? integer? i) `((mov rax ,i))]
    ;[(? boolean? b) `((mov rax ,#b01))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (sub rax 1)))]
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (compile-e e0))
           (c1 (compile-e e1))
           (c2 (compile-e e2))
           (l0 (gensym "if"))
           (l1 (gensym "if")))
       `(,@c0
         (cmp rax 0)
         (jne ,l0)
         ,@c1
         (jmp ,l1)
         ,l0
         ,@c2
         ,l1))]
    
    [`(abs ,exp)
      (let ((c0 (compile-e exp))
           (l0 (gensym)))
      `(,@c0
        (cmp rax 0)
        (jge ,l0)
        (neg rax)
        ,l0))
    ]

    [`(- ,exp)
      (let ((c0 (compile-e exp)))
      `(,@c0
        (neg rax)))
    ]

    [`(zero? ,e0)
      (let ((c0 (compile-e e0))
            (l0 (gensym)))
      `(,@c0
        (cmp rax 0)
        (mov rax #b01) 
        (jne ,l0)
        (mov rax #b11) 
        ,l0
        ))
    ]
    [(list 'cond cs ... `(else ,en))
      (compile-cond-env cs en)
    ]
    ;; TODO
    #;...))

(define (compile-cond-env cs en)
  (match cs
    ['() (compile-e en)]
    [(cons `(,eq ,ea) cs)
      (let ((c0 (compile-e eq))
            (c1 (compile-e ea))
            (c2 (compile-cond-env cs en))
            (l0 (gensym))
            (l1 (gensym))
           )
        `(,@c0
          (cmp rax 1)
          (je ,l0)
          ,@c1
          (jmp ,l1)
          ,l0
          ,@c2
          ,l1
        )
      )
    ])
)
