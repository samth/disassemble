;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (machine-code disassembler arm-private)
  (export
    define-encoding != &= !&=)
  (import
    (rnrs (6))
    (machine-code disassembler private))

  (define != (lambda (x y) (not (= x y))))

  ;; (&= x 'b101x) matches when (member x '(#b1010 #b1011))
  (define-syntax &=
    (lambda (x)
      (syntax-case x (quote)
        [(_ var 'bit-pattern)
         (and (or (identifier? #'var) (integer? (syntax->datum #'var))) (identifier? #'bit-pattern))
         (if (not (char=? #\b (string-ref (symbol->string (syntax->datum #'bit-pattern)) 0)))
             (syntax-violation '&= "Invalid pattern (must be a quoted symbol starting #\\b)" x #'bit-pattern)
             (let ((pattern (symbol->string (syntax->datum #'bit-pattern))))
               (let lp ((i 1) (bits 0) (mask 0))
                 (if (fx=? i (string-length pattern))
                     (if (eqv? mask 0)
                         #'#t
                         #`(eqv? (bitwise-and var #,mask) #,bits))
                     (case (string-ref pattern i)
                       [(#\x)
                        (lp (+ i 1) bits mask)]
                       [(#\0 #\1)
                        (let ((bit (if (eqv? (string-ref pattern i) #\0) 0 1))
                              (idx (- (string-length pattern) i 1)))
                          (lp (+ i 1)
                              (bitwise-ior bits (bitwise-arithmetic-shift-left bit idx))
                              (bitwise-ior mask (bitwise-arithmetic-shift-left 1 idx))))]
                       [else
                        (syntax-violation '&= "Invalid pattern (only x, 0 and 1 are allowed)" x #'bit-pattern)])))))]
        [(_ var bit-pattern)
         #'(eqv? var bit-pattern)])))

  (define-syntax !&=
    (lambda (x)
      (syntax-case x ()
        [(_ var bit-pattern)
         #'(not (&= var bit-pattern))])))

  ;; Syntax for defining instruction encodings. The fields of the
  ;; instructions are written with the index of the top field bit.
  ;; Fields can be given an identifier which will be bound to the
  ;; field value, or a field constraint. This matches what is seen in
  ;; the instruction set encoding chapters of the ARM manuals.

  (define-syntax field-eqv?
    (lambda (x)
      (syntax-case x (!= quote)
        [(_ field (!= value))
         #'(!&= field value)]
        [(_ field value)
         #'(&= field value)])))

  (define-syntax define-encoding
    (lambda (x)
      (define debug #f)
      (define (get-next-top-bit top-bit field-spec*)
        (syntax-case field-spec* ()
          [() -1]
          [((next-top-bit . _) field-spec* ...)
           (and (fixnum? (syntax->datum #'next-top-bit))
                (fx<? (syntax->datum #'next-top-bit) (syntax->datum top-bit)))
           (syntax->datum #'next-top-bit)]))
      (define (wrap-body name lhs* rhs* body)
        (with-syntax ([(lhs* ...) (reverse lhs*)]
                      [(rhs* ...) (reverse rhs*)])
          #`(let ((lhs* rhs*) ...)
              #,(with-syntax ([err #`(raise-UD #,(string-append "Unallocated "
                                                                (symbol->string (syntax->datum name))
                                                                " op")
                                               `(lhs* ,lhs*) ...)])
                  (let f ((body body))
                    (syntax-case body (select match)
                      [(select pc instruction)
                       #'err]
                      [(select pc instruction option option* ...)
                       (if debug
                           #`(let ((x (option pc instruction))
                                   (y (guard (exn ((invalid-opcode? exn) #f))
                                        #,(f #'(select pc instruction option* ...)))))
                               (if (and x y)
                                   (error 'name "Indistinct encoding" pc instruction x y)
                                   (or x y)))
                           #`(or (option pc instruction)
                                 #,(f #'(select pc instruction option* ...))))]
                      [(match (field* ...))
                       #'err]
                      [(match (field* ...) [(value* ...) expr*] . k*)
                       (if debug
                           #`(let ((x (and (field-eqv? field* value*) ...))
                                   (y (guard (exn ((invalid-opcode? exn) #f))
                                        #,(f #'(match (field* ...) . k*)))))
                               (if (and x y)
                                   (error 'name "Indistinct encoding" expr* y)
                                   (if x expr* y)))
                           #`(if (and (field-eqv? field* value*) ...)
                                 expr*
                                 #,(f #'(match (field* ...) . k*))))]))))))
      (syntax-case x ()
        [(_ (encoding-name pc instruction field-spec* ...))
         #'(define-encoding (encoding-name pc instruction field-spec* ...)
             (select pc instruction))]
        [(_ (encoding-name pc instruction field-spec* ...)
            body)
         (and (identifier? #'encoding-name) (identifier? #'instruction))
         (let loop ([field-spec* #'(field-spec* ...)]
                    [eq-mask 0] [eq-bits 0]
                    [neq-mask 0] [neq-bits 0]
                    [lhs* '()] [rhs* '()])
           (syntax-case field-spec* (= !=)
             [()
              (with-syntax ([wrapped-body (wrap-body #'encoding-name lhs* rhs* #'body)])
                (unless (= (bitwise-and eq-bits eq-mask) eq-bits)
                  (syntax-violation 'define-encoding "Bits do not match the mask, bad constraints?"
                                    x field-spec*))
                #`(define (encoding-name pc instruction)
                    (and (eqv? (bitwise-and instruction #,eq-mask) #,eq-bits)
                         (or (eqv? #,neq-mask 0)
                             (not (eqv? (bitwise-and instruction #,neq-mask) #,neq-bits)))
                         wrapped-body)))]
             [((top-bit) field-spec* ...)
              (fixnum? (syntax->datum #'top-bit))
              ;; Ignore anything of the form (<n>).
              (loop #'(field-spec* ...) eq-mask eq-bits neq-mask neq-bits lhs* rhs*)]
             [((top-bit name) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (identifier? #'name))
              ;; Defines a field.
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...))))
                (with-syntax ((accessor #`(bitwise-bit-field instruction
                                                             (+ #,next-top-bit 1) (+ top-bit 1))))
                  (loop #'(field-spec* ...) eq-mask eq-bits neq-mask neq-bits
                        #`(name #,@lhs*) #`(accessor #,@rhs*))))]
             [((top-bit (= field-bits)) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (fixnum? (syntax->datum #'field-bits)))
              ;; Defines a constraint (the field must be equal to field-bits).
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...)))
                     (bottom-bit (fx+ next-top-bit 1))
                     (width (fx+ (fx- (syntax->datum #'top-bit) bottom-bit) 1)))
                (loop #'(field-spec* ...)
                      (bitwise-ior eq-mask
                                   (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 width) 1)
                                                                  bottom-bit))
                      (bitwise-ior eq-bits (bitwise-arithmetic-shift-left (syntax->datum #'field-bits) bottom-bit))
                      neq-mask neq-bits
                      lhs* rhs*))]
             [((top-bit (!= field-bits) name) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (fixnum? (syntax->datum #'field-bits)))
              ;; Defines a field with a constraint (the field must be unequal to field-bits).
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...)))
                     (bottom-bit (fx+ next-top-bit 1))
                     (width (fx+ (fx- (syntax->datum #'top-bit) bottom-bit) 1)))
                (with-syntax ((accessor #`(bitwise-bit-field instruction
                                                             (+ #,next-top-bit 1) (+ top-bit 1))))
                  (loop #'(field-spec* ...)
                        eq-mask eq-bits
                        (bitwise-ior neq-mask
                                     (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 width) 1)
                                                                    bottom-bit))
                        (bitwise-ior neq-bits
                                     (bitwise-arithmetic-shift-left (syntax->datum #'field-bits) bottom-bit))
                        #`(name #,@lhs*) #`(accessor #,@rhs*))))]))]))))
