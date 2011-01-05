#lang racket
(require racket/unsafe/ops "decompiler.rkt")

(define (f x) 
  (and x
       (for/fold () ([i (in-range 100)])
         (values))))

(f 10.0)

(define (g k)
  (if (eq? k 'x) #t (eq? k 'y)))

(provide f g)