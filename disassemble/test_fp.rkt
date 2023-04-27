#lang racket

(require racket/flonum "main.rkt")

(define (add-fp x y)
  (fl+ x y))

(define (cmp-fp x y)
    (let ([a (fl+ x y)]
          [b (fl- x y)])
        (fl= a b)))

(define (hypot a b c)
  (flsqrt (fl+ (fl* a a) (fl* b b) (fl* c c))))

(void (decompile add-fp))
(newline)
(void (decompile cmp-fp))
(newline)
(void (decompile hypot))
