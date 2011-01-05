#lang racket

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

(define command "ndisasm -b 32 ")
;(define command "x86dis -e 0 -f ")

(define (nasm-disassemble-bytevector bv)
  (let ([tempfile (make-temporary-file "nasmtemp~a.o")])
    (let ((out (open-output-file tempfile #:exists 'truncate)))
      (write-bytes bv out)
      (flush-output out)
      (close-output-port out))
    (with-output-to-string
     (λ () (system (string-append command (path->string tempfile)))))))

#;
(define (nasm-disassemble-procedure p)
  (nasm-disassemble-bytevector (procedure-ref p 0)))

(define (nasm-disassemble x)
  (cond
    #;
   ((procedure? x) 
    (nasm-disassemble-procedure x))
   ((bytes? x)
    (nasm-disassemble-bytevector x))
   (else
    (error 'nasm-disassemble "Unknown input type ~a" x))))

(define b (bytes #x65 #x67 #x89 #x87 #x76 #x65 #x54 #x56 #x78 #x89 #x09 #x00 #x87))
#;#;
(nasm-disassemble b)

(nasm-disassemble #"\211C\374\203\303\374\213C\4\203\300\2\203\304\34_^[]\303\303\0\a\0\0\0\20\27\262\210I\3\262\360\345\353\237\0\0\0\0\0\0\0\0U\211\345SVW\213E\b\213M\f\213U\20\203\354\34")

