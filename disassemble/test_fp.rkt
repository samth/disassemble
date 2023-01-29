#lang racket

(require racket/flonum "main.rkt")

(define (add-fp x y)
  (fl+ x y))

(define (cmp-fp x y)
    (let ([a (fl+ x y)]
          [b (fl- x y)])
        (fl= a b)))

(void (decompile add-fp))
(void (decompile cmp-fp))