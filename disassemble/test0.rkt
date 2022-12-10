#lang racket

(require "main.rkt")

(define (iter f n i)
  (cond
    [(= n 0) i]
    [else (f (iter f (sub1 n) i))]))

(define (fact n)
	(if (<= n 0) 1
		(* n (fact (- n 1)))))

(decompile fact)