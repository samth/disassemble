#lang racket
(require disassemble racket/unsafe/ops)
(define (f x) (+ x 7))
(set! f f)

(define (g x) (unsafe-fx+ x 7))
(set! g g)

(define (h x)
  (for/sum ([i (in-range x)]) i))
;(set! h h)

(module+ main
  (f 5)
  (g 5)
  (h 500)
  (decompile f)
  (decompile g)
  (decompile h))
 
