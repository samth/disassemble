;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Generic disassembler support.

(library (machine-code disassembler)
  (export
    invalid-opcode?
    available-disassemblers get-disassembler
    disassembler? disassembler-name
    disassembler-min-instruction-size
    disassembler-max-instruction-size
    disassembler-instruction-getter)
  (import
    (rnrs (6))
    (rename (machine-code disassembler private)
            (available-disassemblers p:available-disassemblers)
            (get-disassembler p:get-disassembler))
    #;(prefix (machine-code disassembler arm-a32) arm-a32:)
    (prefix (machine-code disassembler arm-a64) arm-a64:)
    #;(prefix (machine-code disassembler i8080) i8080:)
    #;(prefix (machine-code disassembler m68hc12) m68hc12:)
    #;(prefix (machine-code disassembler mips) mips:)
    (prefix (machine-code disassembler x86) x86:))

  (define register-all-disassemblers
    (let ((done #f))
      (lambda ()
        (unless done
          ;; visit libraries
          (set! done (list x86:get-instruction
                           #;mips:get-instruction
                           #;m68hc12:get-instruction
                           #;i8080:get-instruction
                           #;arm-a32:get-instruction
                           arm-a64:get-instruction))))))

  (define (available-disassemblers)
    (register-all-disassemblers)
    (p:available-disassemblers))

  (define (get-disassembler name)
    (register-all-disassemblers)
    (or (p:get-disassembler name)
        (error 'get-disassembler "This disassembler has not been registered" name))))
