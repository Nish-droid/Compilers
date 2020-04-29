#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (+ (interp e0) 1)]
    [`(sub1 ,e0)
     (- (interp e0) 1)]

    [`(if (zero? ,e0) ,e1 ,e2)
     (if (zero? (interp e0))
         (interp e1)
         (interp e2))]
    
    [`(abs ,e0)
     (if (> (interp e0) 0) e0
         (- e0))
     ]

    [`(- ,e0)
     (- 0 (interp e0))
     ]

    [`(else ,e0)
     (interp e0)
     ]
 
    [`(cond
        [else ,e0]) (interp e0)
                    ]
    [`((zero? ,e0) ,e1)
      (if (zero? (interp e0)) (interp e1) -1)
    ]

    [`([(zero? ,e0) ,e1] ,e2)
     (if (zero? (interp e0)) (interp e1) (interp e2))
     ]

    [`(cond [(zero? ,e0) ,@e1] ,@e2)
     (if (zero? (interp e0)) (interp e1) (interp e2))
     ]
    
    [(cons e1 e2)
      (interp e1)
    ]
    )
  )
