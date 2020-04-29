#lang racket
;(require asm/generic)
(provide (all-defined-out))
 

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp) ; push before calling
    (call error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i)
     `((mov rax ,(* i 4)))
    ]

    [(? boolean? b)
     `((mov rax ,(if b #b101 #b001)))
    ]
    
    [(? char? c)
      ;`((mov rax #b110000110))
      `((mov rax ,(+ (* (char->integer c) 4) 2)))
      ;`((mov rax #b110000100))
    ]

    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax 4)))
    ]

    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax 4)))
    ]

    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c))
           (l0 (gensym)))
       `(,@c0
         ,@assert-integer
         (cmp rax ,0)
         (mov rax ,#b001) ; #f         
         (jne ,l0)
         (mov rax ,#b101) ; #t
         ,l0
         ))
    ]

    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b001) ; compare to #f
         (je ,l0)        ; jump to c2 if #f
         ,@c1
         (jmp ,l1)       ; jump past c2
         ,l0
         ,@c2
         ,l1))
    ]

    [`(abs ,exp)
      (let ((c0 (compile-e exp c))
           (l0 (gensym)))
      `(,@c0
        ,@assert-integer
        (cmp rax ,0)
        (jge ,l0)
        (neg rax)
        ,l0))
    ]

    [`(- ,exp)
      (let ((c0 (compile-e exp c)))
      `(,@c0
        ,@assert-integer
        (neg rax)))
    ]

    [(list 'cond cs ... `(else ,en))
      (compile-cond-env cs en c)
    ]

    ;; TODO: make sure this works with your generalized let
    [(? symbol? x)
      (let ((i (lookup x c)))
          `((mov rax (offset rsp ,(- (add1 i)))))
      )
    ]
        
    [(list 'char? v0)
      (let ((c0 (compile-e v0 c))
           (l0 (gensym))
           (l1 (gensym)))
        `(,@c0
            (mov rbx rax)
            (and rbx #b10)
            (cmp rbx #b10)
            (jne ,l0)
            (mov rax #b101)
            (jmp ,l1)
            ,l0
            (mov rax #b001)
            ,l1
         )
      )
      ;`((mov rax ,(if (char? v0) #b101 #b001)))
    ]

    [(list 'integer? v0)
      (let ((c0 (compile-e v0 c)))
         `(,@c0
            (mov rax ,(if (integer? v0) #b101 #b001))
          )
      )
    ]

    [(list 'boolean? v0)
      (let ((c0 (compile-e v0 c))
            (l0 (gensym "bool"))
            (l1 (gensym)))
        `(,@c0
            (cmp rax ,#b101)
            (je ,l0)
            (cmp rax ,#b001)
            (je ,l0)
            (mov rax ,#b001)
            (jmp ,l1)
            ,l0
            (mov rax ,#b101)
            ,l1
         )
      )
    ]

    [(list 'integer->char i0)
        (let ((c0 (compile-e i0 c))
              (l0 (gensym))
              (l1 (gensym)))
            `(,@c0
              ,@assert-integer
              (add rax ,2)   ;(mov rax ,(+ (* i0 4) 2))
              (cmp rax ,-1)
              (jle ,l0)
              (cmp rax ,(+ (* (add1 #xDFFF) 4) 2))
              (jge ,l1)
              (cmp rax ,(+ (* (sub1 #xD800) 4) 2))
              (jle ,l1)
              ,l0
              (jmp err);((call error))  
              ,l1
            )
        )
    ]

    [(list 'char->integer ch) 
      (let ((c0 (compile-e ch c)))
         `(,@c0
            ,@assert-char
            (sub rax ,2)
          )
      ) 
    ]

     ;; TODO: generalize
    [`(let ,(list `(,xs ,es) ...) ,e)
      (let ((c0 (compile-envs xs es c))
            (c1 (compile-e e (append (reverse xs) c))))
        `(,@c0
            ,@c1
         )
      )
    ]

     [`(let* ,(list `(,xs ,es) ...) ,e)
      (let ((c0 (compile-envs2 xs es c))
            (c1 (compile-e e (append (reverse xs) c))))
            `(,@c0
              ,@c1
            )
      )
    ]
    
    #;...))

;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (compile-envs xs es r)
  (match es
    ['() '()]
    [(cons e es)
      (let ((c0 (compile-e e r)))
        `(,@c0
           (mov (offset rsp ,(- (add1 (length r)))) rax)
         
          ,@(compile-envs xs es (append '(xs) r))
         )
      )   
    ]
  )
)

(define (compile-envs2 xs es r)
  (match es
    ['() '()]
    [(cons e es)
      (let ((c0 (compile-e e (append (list (first xs)) r))))
        `(,@c0
           (mov (offset rsp ,(- (add1 (length r)))) rax)
          ,@(compile-envs2 (rest xs) es (append (list (first xs)) r))
         )
      )   
    ]
  )
)

(define (compile-cond-env cs en r)
  (match cs
    ['() (compile-e en r)]
    [(cons `(,eq ,ea) cs)
      (let ((c0 (compile-e eq r))
            (c1 (compile-e ea r))
            (c2 (compile-cond-env cs en r))
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

;; Asm
(define assert-integer
  `((mov rbx rax)
    (and rbx #b11)
    (cmp rbx 0)
    (jne err))
)

(define assert-boolean
 `((mov rbx rax)
    (and rbx #b01)
    (cmp rbx #b01)
    (jne err) 
  )
)

(define assert-char
  `((mov rbx rax)
   (and rbx #b10)   ;#b10 #b11(works with #b10) #b01(fails) (cmp rbx #b11)
   (cmp rbx #b10)
   (jne err))
)

(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
      (match (symbol=? x y)
        [#t (length cenv)]
        [#f (lookup x cenv)])]))
