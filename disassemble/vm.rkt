#lang racket/base
(require racket/linklet)

;; Same functions as `ffi/unsafe/vm`, but avoiding (for now) a dependency
;; on the latest Racket

(provide vm-eval
         vm-primitive)

(define-values (vm-eval vm-primitive)
  (case (system-type 'vm)
    [(chez-scheme)
     (define-values (raw-eval call-with-system-wind)
       (instantiate-linklet
        (compile-linklet '(linklet () () (values eval call-with-system-wind)))
        '()
        (make-instance 'top-level)))
     (define (eval s)
       (call-with-system-wind
        (lambda ()
          (raw-eval s))))
     (values eval eval)]
    [else
     (values #f #f)]))
