#lang at-exp racket
(require rackunit rackunit/text-ui recspecs
         racket/unsafe/ops
         "../disassemble/main.rkt" "../disassemble/pb.rkt")

(define nasm-exe
  (case (system-type)
    [(windows) "ndisasm.exe"]
    [else "ndisasm"]))
(define nasm-available? (find-executable-path nasm-exe))
(define x86-64? (eq? (system-type 'arch) 'x86_64))

(define disassemble-tests
  (test-suite
   "disassemble-tests"
   @expect[(disassemble-bytes (bytes #x90 #xc3) #:arch 'x86-64)]{
     0: 90                             (nop)
     1: c3                             (ret)
   }
   (when nasm-available?
     @expect[(disassemble-bytes (bytes #x90 #xc3)
                                 #:arch 'x86-64
                                 #:program 'nasm)]{
       00000000  90                nop
       00000001  C3                ret
     })
   @expect[(pb-disassemble (bytes 0 0 0 0) (pb-config 32 'little #f) '())]{
     0:	00000000	(nop)
   }
   @expect[(pb-disassemble (bytes #xd5 0 0 0) (pb-config 32 'little #f) '())]{
     0:	000000d5	(return)
   }
   @expect[(pb-disassemble (bytes #xd7 0 0 0) (pb-config 32 'little #f) '())]{
     0:	000000d7	(adr %tc (imm #x0))
   }
   @expect[(pb-disassemble (bytes #xd6 0 0 0) (pb-config 32 'little #f) '())]{
     0:	000000d6	(interp %tc)
   }
   @expect[(pb-disassemble (bytes #x01 #x01 0 0 0 0 0 0)
                            (pb-config 32 'little #f) '())]{
     0:	00000101	(literal %sfp)
     4:	00000000	(data)
   }
  ;; The exact machine code Racket CS emits for a compiled procedure
  ;; depends on the Chez Scheme / Racket version, so these check the
  ;; disassembler's output structurally rather than against an exact
  ;; snapshot (which would break across the versions tested in CI). A
  ;; compiled procedure begins with an arity check against the argument
  ;; register `rbp`, so `cmp ... rbp` is a stable marker.
  (when x86-64?
    (define (disasm-string proc #:program [program #f])
      (with-output-to-string
        (lambda ()
          (if program
              (disassemble proc #:arch 'x86-64 #:program program)
              (disassemble proc #:arch 'x86-64)))))
    (test-case "disassemble function"
      (define (ret1) 1)
      (define out (disasm-string ret1))
      (check-pred non-empty-string? out)
      (check-regexp-match #rx"cmp" out)
      (check-regexp-match #rx"rbp" out))
    (test-case "disassemble fx-add"
      (define (fx-add x y)
        (unsafe-fx+ x y))
      (define out (disasm-string fx-add))
      (check-pred non-empty-string? out)
      (check-regexp-match #rx"cmp" out))
    (test-case "disassemble uses-const-string"
      (define const-string "a constant string")
      (define (uses-const-string)
        (display const-string))
      (define out (disasm-string uses-const-string))
      (check-pred non-empty-string? out)
      (check-regexp-match #rx"cmp" out))
    (when nasm-available?
      (test-case "disassemble function nasm"
        (define (ret1) 1)
        (define out (disasm-string ret1 #:program 'nasm))
        (check-pred non-empty-string? out)
        (check-regexp-match #rx"cmp rbp" out))))
   ))

(module+ test
  (unless (zero? (run-tests disassemble-tests))
    (error 'disassemble-tests "test suite failed")))
