#lang racket
(require disassemble racket/unsafe/ops)
(define (f x) (+ x 7))

(define (g x) (unsafe-fx+ x 7))


(define (h x)
  (for/sum ([i (in-range x)]) i))

(module+ test
  (decompile f)
  (decompile g)
  (decompile h))
 
