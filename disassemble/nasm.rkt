#lang racket/base

;; avoid test errors when nasm isn't available
(module* test racket/base)

(require racket/file racket/port racket/system racket/match)

(provide nasm-disassemble)

;; Taken from Larceny, written by Felix Klock

;;; Experimental library to get us semi-reliable IA32 disassembly by
;;; delegating the job to the nasm disassembler ndisasm.
;;;
;;; Note that the output can be misleading especially because we
;;; currently encode exception codes directly in the instruction
;;; stream, which the disassembler has no knowledge of. (To find this
;;; in the IAssassin backend, just search for the token 'dwords'; that
;;; is the directive for emitting constants in Sassy.)

(define nasm-path (find-executable-path "ndisasm"))

(unless nasm-path
  (error 'disassemble "unable to find the `ndisasm' executable"))

(define nasm-help-text
  (let ()
    (define p (open-output-string))
    (match-define (list _ in pid _ proc) (process*/ports p #f 'stdout nasm-path "-h"))
    (proc 'wait)
    (close-output-port in)
    (get-output-string p)))

(define systype (if (fixnum? (expt 2 61)) 64 32))

(unless (regexp-match (regexp-quote (number->string systype))
                      nasm-help-text)
  (error 'nasm "this version of ndisasm does not support ~a-bit disassembly" systype))

(define (nasm-disassemble-bytevector bv)
  (let ([tempfile (make-temporary-file "nasmtemp~a.o")])
    (let ((out (open-output-file tempfile #:exists 'truncate)))
      (write-bytes bv out)
      (flush-output out)
      (close-output-port out))
    (define p (open-output-string))
    (match-define (list _ in pid _ proc)
                  (process*/ports p #f 'stdout nasm-path "-b" 
                                  (number->string systype)
                                  (path->string tempfile)))
    (proc 'wait)
    (delete-file tempfile)
    (close-output-port in)
    (get-output-string p)))

(define (nasm-disassemble x)
  (cond
   ((bytes? x)
    (nasm-disassemble-bytevector x))
   (else
    (raise-argument-error 'nasm-disassemble "bytes" x))))

