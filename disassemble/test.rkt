#lang racket
(require racket/unsafe/ops "main.rkt")

(define (f x)
  (and x
       (for/fold () ([i (in-range 100)])
         (values))))

(f 10.0)

(define (g k)
  (if (eq? k 'x) #t (eq? k 'y)))

(provide f g)


(define z 100)
(define f2 (let ([cnt 0]) (lambda (x y z) (set! cnt (random 100)) (* 3 x cnt z))))

(length
 (for/list ([i (in-range 100)])
   (set! z (random 1000))
   (f2 1 2 4)))

(decompile f2)
(define x (case-lambda [(x) 1] [(x y) (list x y)]))
(define (y [x 1] [y 2] [z 3] [w 4] [a 5] [b 6]) 1)
(decompile x)
(decompile y)
(define (id x)
  (for/fold ([z 0.0]) ([i (in-range 100)])
    (unsafe-fl+ x z)))

(void (id 2.0))

(void (decompile id))
