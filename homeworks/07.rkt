#lang racket

(define ones
    (cons 1 (thunk ones)))

(define naturals
    (let loop ([i 1])
        (cons i (thunk (loop (add1 i))))))

(define fibs
    (let loop ([a 1] [b 0])
        (cons a (thunk (loop (+ a b) a)))))

(define (first n tok)
    (if (= n 0) null
        (cons (car tok) 
              (first (- n 1) ((cdr tok))))))

(define (squares tok)
    (cons (* (car tok) (car tok))
          (thunk (squares ((cdr tok))))))

(define (partitions k n)
    (cond
        [(and (= n 0) (= k 0)) 1]
        [(<= n 0) 0]
        [(<= k 0) 0]
        [else (+ (partitions k (- n k)) (partitions (- k 1) (- n 1)))]))

(define-syntax sml
  (syntax-rules (:: hd tl null nil)
    [(sml x :: xs) (cons x xs)]
    [(sml hd lst) (car lst)]
    [(sml tl lst) (cdr lst)]
    [(sml null lst) (null? lst)]
    [(sml nil) '()]))

(define (my-delay thunk) 0)

(define (my-force delayed)
  (delayed))