#lang racket

(require "main.rkt")

(define (iter f n i)
  (cond
    [(= n 0) i]
    [else (f (iter f (sub1 n) i))]))

(define (fact n)
	(if (<= n 0) 1
		(* n (fact (- n 1)))))

(define (func x)
    (define tmp 3)
    (if (= x 0)
        (set! tmp 0)
        (set! tmp 1))
    tmp)

(decompile fact)
(decompile func)
;(decompile iter)
