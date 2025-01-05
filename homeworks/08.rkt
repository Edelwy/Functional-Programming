#lang racket

(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct ?leq (a b) #:transparent)
(struct ~(a) #:transparent)
(struct ?int(a) #:transparent)
(struct if-then-else(condition a b) #:transparent)

(define (bool? value) 
    (cond 
        [(false? value) #t]
        [(true? value) #t]
        [else #f]))

(define (fri expr)
    (cond
        [(int? expr) expr]                                  ;; If the expression is of int value return the value.
        [(true? expr) expr]                                 ;; If the expression is true return true.
        [(false? expr) expr]                                ;; If the expression is false return false.
        [(add? expr)                                        ;; If the expression is 'add' evaluate it.            
            (let ([a (fri (add-a expr))]
                  [b (fri (add-b expr))])
                (cond
                    [(and (bool? a) (bool? b)) (if (or (true? a) (true? b)) (true) (false))]
                    [(and (int? a) (int? b)) (int (+ (int-n a) (int-n b))) ]
                    [else (error "Illegal expression.")]
                )
            )
        ]
        [(mul? expr)                                        ;; If the expression is 'mul' evaluate it.            
            (let ([a (fri (mul-a expr))]
                  [b (fri (mul-b expr))])
                (cond
                    [(and (bool? a) (bool? b)) (if (and (true? a) (true? b)) (true) (false))]
                    [(and (int? a) (int? b)) (int (* (int-n a) (int-n b))) ]
                    [else (error "Illegal expression.")]
                )
            )
        ]
        [(?leq? expr)                                        ;; If the expression is '?leq' evaluate it.            
            (let ([a (fri (?leq-a expr))]
                  [b (fri (?leq-b expr))])
                (cond
                    [(and (bool? a) (bool? b)) (if (and (true? a) (false? b)) (false) (true))]
                    [(and (int? a) (int? b)) (if (<= (int-n a) (int-n b)) (true) (false))]
                    [else (error "Illegal expression.")]
                )
            )
        ]
        [(~? expr)                                           ;; If the expression is '~' evaluate it.            
            (let ([a (fri (~-a expr))])
                (cond
                    [(bool? a) (if (true? a)(false) (true))]
                    [(int? a) (int (- (int-n a)))]
                    [else (error "Illegal expression.")]
                )
            )
        ] 
        [(?int? expr)                                        ;; If the expression is of int type return true else false.           
            (let ([a (fri (?int-a expr))])
                (if (int? a) (true) (false))
            )
        ]
        [(if-then-else? expr)                                           ;; If the expression is a branch resolve it.            
            (let ([condition (fri (if-then-else-condition expr))])
                (cond
                    [(true? condition) (fri (if-then-else-a expr))]
                    [(false? condition) (fri (if-then-else-b expr))]
                    [else (error "Illegal expression.")]
                )
            )
        ]
    )
)

(define-syntax conditional
    (syntax-rules ()
        ((_ condition a b) (if-then-else condition a b))
        ((_ condition a b c ...)
            (if-then-else condition a (conditional b c ...)))))

(define-syntax-rule (?geq a b)
    (?leq b a))
