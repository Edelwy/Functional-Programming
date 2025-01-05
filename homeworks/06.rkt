#lang racket

(define (power x n)
    (if (= n 0) 1 (* x (power x (- n 1)))))

(define (gcd a b)
    (cond 
        [(= a 0) 0]
        [(> a b) (gcd b (- a b))]
        [(> b a) (gcd a (- b a))]
        [else a]))

(define (fib n)
    (cond
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (reverse lst)
    (if (empty? lst) null 
        (append (reverse (rest lst)) (cons (first lst) null))))

(define (remove x lst)
    (if (empty? lst) null 
        (if (= x (first lst)) (remove x (rest lst)) 
            (cons (first lst) (remove x (rest lst))))))
        
(define (map f lst)
    (if (empty? lst) null 
        (cons (f (first lst)) (map f (rest lst)))))

(define (filter f lst)
    (if (empty? lst) null
        (if (f (first lst)) (cons (first lst) (filter f (rest lst))) 
            (filter f (rest lst)))))

(define (zip lst kst)
    (cond
        [(empty? lst) null]
        [(empty? kst) null]
        [else (cons 
            (cons (first lst) (first kst)) 
            (zip (rest lst) (rest kst)))]))

(define (range a b k)
    (if (> a b) null
        (cons a (range (+ a k) b k))))

(define (is-palindrome lst)
    (equal? lst (reverse lst)))