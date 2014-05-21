#lang racket/base

(require racket/file racket/port racket/system)

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

(define command
  (format "ndisasm -b ~a " (if (fixnum? (expt 2 61)) 64 32)))

(define (nasm-disassemble-bytevector bv)
  (let ([tempfile (make-temporary-file "nasmtemp~a.o")])
    (let ((out (open-output-file tempfile #:exists 'truncate)))
      (write-bytes bv out)
      (flush-output out)
      (close-output-port out))
    (with-output-to-string
      (λ () (system (string-append command (path->string tempfile)))))))

(define (nasm-disassemble x)
  (cond
   ((bytes? x)
    (nasm-disassemble-bytevector x))
   (else
    (raise-argument-error 'nasm-disassemble "bytes" x))))

