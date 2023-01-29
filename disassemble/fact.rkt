#lang racket

(define (fact n)
	(if (<= n 0) 1
		(* n (fact (- n 1)))))


(define (func x)
    (define tmp 3)
    (if (= x 0)
        (set! tmp 0)
        (set! tmp 1))
    tmp)