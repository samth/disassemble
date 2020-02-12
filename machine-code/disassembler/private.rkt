;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012, 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Code shared between the disassemblers. Should not be imported by
;; anyone else.

(library (machine-code disassembler private)
  (export
    raise-UD invalid-opcode? map-in-order
    register-disassembler
    available-disassemblers get-disassembler
    make-disassembler disassembler? disassembler-name
    disassembler-min-instruction-size
    disassembler-max-instruction-size
    disassembler-instruction-getter)
  (import
    (rnrs (6)))

  (define (map-in-order p l)
    (if (null? l)
        '()
        (cons (p (car l))
              (map-in-order p (cdr l)))))

  (define-condition-type &invalid-opcode &condition
    make-invalid-opcode invalid-opcode?)

  (define (raise-UD msg . irritants)
    (raise (condition
            (make-who-condition 'get-instruction)
            (make-message-condition msg)
            (make-irritants-condition irritants)
            (make-invalid-opcode))))

  (define-record-type disassembler
    (fields name
            min-instruction-size
            max-instruction-size
            instruction-getter))

  (define *registered-disassemblers* '())

  (define (register-disassembler disassembler)
    (set! *registered-disassemblers* (cons (cons (disassembler-name disassembler)
                                                 disassembler)
                                           *registered-disassemblers*)))

  (define (available-disassemblers)
    (map car *registered-disassemblers*))

  (define (get-disassembler name)
    (cond ((assq name *registered-disassemblers*) => cdr)
          (else #f))))
