#lang racket
(require racket/unsafe/ops "main.rkt")

(define (f x)
  (and x
       (for/fold () ([i (in-range 100)])
         (values))))

(define (g k)
  (if (eq? k 'x) #t (eq? k 'y)))


(define z 100)
(define f2 (let ([cnt 0]) (lambda (x y z) (set! cnt (random 100)) (* 3 x cnt z))))


(void (decompile f2))
(define x (case-lambda [(x) 1] [(x y) (list x y)]))
(define (y [x 1] [y 2] [z 3] [w 4] [a 5] [b 6]) 1)

(define (id x)
  (for/fold ([z 0.0]) ([i (in-range 100)])
    (unsafe-fl+ x z)))

(void (decompile id))
