#lang racket

(require (for-syntax racket/syntax))
(provide pb-disassemble)

(define pb-instruction-byte-size 4)

;; PB instruction shapes. All instructions are 4 bytes in length
;;  -----------------------------------------------
;;  |    op    |    reg    |     immed/reg        |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    | reg | reg |     immed/reg        |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    | reg |          immed             |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    |             immed                |
;;  -----------------------------------------------

(define-syntax (define/enum stx)
  (syntax-case stx ()
    [(_ name fields ...)
        (let* ([field-names (syntax->datum #'(fields ...))]
              [field-symbols (map (lambda (field) (string->symbol (format "~a" field))) field-names)])
          (with-syntax ([struct-name (format-id stx "~a-struct" (syntax-e #'name))]
                        [instance-name (format-id stx "~a" (syntax-e #'name))])
            #`(begin
                (struct struct-name [enum-fields] #:transparent)
                (define instance-name (struct-name '#,field-symbols))

                #,(let loop ([fields* (syntax->datum #'(fields ...))] [i 0])
                  (cond
                    [(null? fields*) (void)]
                    [else
                     (with-syntax ([const-name (datum->syntax stx (string->symbol (format "~a" (car fields*))))])
                       #`(begin
                           (define const-name #,i)
                           #,(loop (cdr fields*) (+ 1 i))))])))))]))

(define-syntax (enum-fields stx)
  (syntax-case stx ()
    [(_ enum-name)
     (with-syntax ([accessor-name (format-id stx "~a-struct-enum-fields" (syntax-e #'enum-name))])
       #'(accessor-name enum-name))]))

(define-syntax (enum-field-count stx)
  (syntax-case stx ()
    [(_ enum-name)
        #`(length (enum-fields enum-name))]))

(define pb-binop-group-start 22)

(define pb-mov-16-group-start 2)
(define pb-mov-16-group-count 8)
(define pb-mov-group-start 10)
(define pb-cmp-group-start 74)
(define pb-fp-binop-group-start 92)

(define/enum pb-argument-types
    pb-register
    pb-immediate)

(define/enum pb-shift
    pb-shift0
    pb-shift1
    pb-shift2
    pb-shift3)

(define/enum pb-zk
    zero-bits
    keep-bits)

(define/enum pb-mov-types
    pb-i->i
    pb-d->d
    pb-i->d
    pb-d->i
    pb-s->d
    pb-d->s
    pb-d->s->d
    pb-i-bits->d-bits     ; 64-bit only
    pb-d-bits->i-bits     ; 64-bit only
    pb-i-i-bits->d-bits   ; 32-bit only
    pb-d-lo-bits->i-bits  ; 32-bit only
    pb-d-hi-bits->i-bits)

(define/enum pb-signal-types
    pb-no-signal
    pb-signal)

(define/enum pb-regs
    pb-reg-tc
    pb-reg-sfp
    pb-reg-ap
    pb-reg-trap
    pb-reg-ac0
    pb-reg-xp
    pb-reg-ts
    pb-reg-td
    pb-reg-cp
    pb-reg-r9
    pb-reg-r10
    pb-reg-r11
    pb-reg-r12
    pb-reg-r13
    pb-reg-r14
    pb-reg-r15
    pb-reg-fp1
    pb-reg-fp2
    pb-reg-fp3
    pb-reg-fp4
    pb-reg-fp5
    pb-reg-fp6
    pb-reg-fp7
    pb-reg-fp8)

(define pb-mov-type-names
    (vector
        "i->i"
        "d->d"
        "i->d"
        "d->i"
        "s->d"
        "d->s"
        "d->s->d"
        "i-bits->d-bits"
        "d-bits->i-bits"
        "i-i-bits->d-bits"
        "d-lo-bits->i-bits"
        "d-hi-bits->i-bits"))

  (define/enum pb-binaries
    pb-add
    pb-sub
    pb-mul
    pb-div
    pb-subz
    pb-subp
    pb-and
    pb-ior
    pb-xor
    pb-lsl
    pb-lsr
    pb-asr
    pb-lslo)

(define pb-binop-names
    (vector
        "add"
        "sub"
        "mul"
        "div"
        "subz"
        "subp"
        "and"
        "ior"
        "xor"
        "lsl"
        "lsr"
        "asr"
        "lslo"))

(define pb-fp-binop-names 
    (vector-map (lambda (n) (format "fp-~a" n)) pb-binop-names))

(define/enum pb-cmp-ops
    pb-eq
    pb-lt
    pb-gt
    pb-le
    pb-ge
    pb-ab
    pb-bl
    pb-cs
    pb-cc)

(define pb-cmp-names 
    (vector
        "eq"
        "lt"
        "gt"
        "le"
        "ge"
        "ab"
        "bl"
        "cs"
        "cc"))

(define reg-names (vector
                "tc"
                "sfp"
                "ap"
                "trap"
                "ac0"
                "xp"
                "ts"
                "td"
                "cp"
                "r9"
                "r10"
                "r11"
                "r12"
                "r13"
                "r14"
                "r15"
                "fp1"
                "fp2"
                "fp3"
                "fp4"
                "fp5"
                "fp6"
                "fp7"
                "fp8"))

(define (format-instr-parts op-name properties)
    (format "(~s ~s)" op-name (string-join properties " ")))

(define (format-reg r)
    (let ([reg-name (vector-ref reg-names r)])
                (format "%~a" reg-name)))

(define (format-imm imm)
    (format "(imm #x~x)" imm))

(define (format-instr/dri op-name dest reg imm props)
    (format "(~a ~a ~a ~a ~a)" op-name (format-reg dest) (format-reg reg) (format-imm imm) (string-join (map format-prop props))))

(define (format-instr/di op dst imm props)
    (format "(~a ~a ~a ~a)" 
        op
        (format-reg dst)
        (format-imm imm)
        (string-join (map format-prop props))))


(define (format-instr/dr op dst reg props)
    (format "(~a ~a ~a ~a)"
        op
        (format-reg dst)
        (format-reg reg)
        (string-join (map format-prop props))))

(define (format-instr/drr op dst r1 r2 props)
    (format "(~a ~a ~a ~a ~a)"
        op
        (format-reg dst)
        (format-reg r1)
        (format-reg r2)
        (string-join (map format-prop props))))

(struct zero/keep [zk])
(struct shift [s])
(struct mov-type [mt])
(struct signal [s])

(define (format-prop prop)
    (match prop
        [(zero/keep zk) (format "#:zk ~a" zk)]
        [(shift s) (format "#:shift ~a" s)]
        [(mov-type mt) (format "#:mov-type ~a" (vector-ref pb-mov-type-names mt))]
        [(signal s) (format "#:signal ~a" (equal? s pb-signal))]))

(define (format-zero/keep z/k)
    (match z/k
        [pb-zero-bits "zero"]
        [pb-keep-bits "keep"]))

(define (format/pb-mov16 s zk reg imm)
    (format-instr/di
        "pb-mov-16"
        reg
        imm
        (list (zero/keep zk)
              (shift s))))

(define (decode/pb-mov16 instr)
    (let* ([op (instr-op instr)]
           [rel (- op pb-mov-16-group-start)]
           [shift (remainder rel 4)]
           [zero/keep (quotient rel 4)]
           [reg (instr-di-dest instr)]
           [imm (instr-di-imm instr)])
           (format/pb-mov16 shift zero/keep reg imm)))

(define (decode/pb-mov instr)
    (let*
        ([op (instr-op instr)]
         [rel (- op pb-mov-group-start)]
         [movt (remainder rel (enum-field-count pb-mov-types))]
         [reg (instr-dr-reg instr)]
         [dst (instr-dr-dest instr)])
         (cond 
            [(equal? mov-type pb-i-i-bits->d-bits)
                "(unsupported)"]
            [else (format-instr/dr "pb-mov" dst reg (list (mov-type movt)))])))

(define (decode/pb-binop instr)
    (let*
        ([op (instr-op instr)]
         [rel (- op pb-binop-group-start)]
         [drr/dri (remainder rel (enum-field-count pb-argument-types))]
         [op-kind (remainder (quotient rel (enum-field-count pb-signal-types)) 
                                (enum-field-count pb-binaries))]
         [sig (quotient rel (* (enum-field-count pb-binaries) (enum-field-count pb-signal-types)))])
         (cond
            ; drr
            [(equal? drr/dri 0) 
                (format-instr/drr 
                    (vector-ref pb-binop-names op-kind)
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    (list (signal sig)))]
            ; dri
            [(equal? drr/dri 1)
                (format-instr/dri 
                    (vector-ref pb-binop-names op-kind)
                    (instr-dri-dest instr)
                    (instr-dri-reg instr)
                    (instr-dri-imm instr)
                    (list (signal sig)))])))

(define (decode/pb-cmp instr)
    (let* ([op (instr-op instr)]
           [rel (- op pb-cmp-group-start)] 
           [dr/di (remainder rel (enum-field-count pb-argument-types))]
           [op-kind (remainder (quotient rel (enum-field-count pb-argument-types)) 
                                            (enum-field-count pb-cmp-ops))])
        (cond 
            ; dr
            [(equal? dr/di 0) 
                (format-instr/dr 
                    (vector-ref pb-cmp-names op-kind)
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '())]
            ; di
            [(equal? dr/di 1) 
                (format-instr/di
                    (vector-ref pb-cmp-names op-kind)
                    (instr-di-dest instr)
                    (instr-di-imm instr)
                    '())])))

(define (decode/pb-fp-binop instr)
    (let* ([op (instr-op instr)]
        [rel (- op pb-binop-group-start)]
        [drr/dri (remainder rel (enum-field-count pb-argument-types))]
        [op-kind (remainder (quotient rel (enum-field-count pb-argument-types)) 
                            (enum-field-count pb-binaries))])
        (cond 
            [(equal? drr/dri 0)
                (format-instr/drr
                    (vector-ref pb-fp-binop-names op-kind)
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    '())]
            [else (error 'pb-disassemble "floating-point instruction cannot have dri variant")])))

(define (instr-op instr) (bitwise-and instr #xFF))

(define (instr-d-dest instr) (bitwise-and (arithmetic-shift instr -8) #xF))

(define (instr-dr-dest instr) (instr-d-dest instr))
(define (instr-dr-reg instr) (bitwise-and (arithmetic-shift instr -16) #xF))

(define (instr-di-dest instr) (instr-d-dest instr))
(define (instr-di-imm instr) (arithmetic-shift instr -16))

(define (instr-adr-dest instr) (instr-di-dest instr))
(define (instr-adr-imm instr) (arithmetic-shift instr -12))

(define (instr-drr-dest instr) (instr-d-dest instr))
(define (instr-drr-reg1 instr) (bitwise-and (arithmetic-shift instr -12) #xF))
(define (instr-drr-reg2 instr) (bitwise-and (arithmetic-shift instr -16) #xF))

(define (instr-dri-dest instr) (instr-d-dest instr))
(define (instr-dri-reg instr) (bitwise-and (arithmetic-shift instr -12) #xF))
(define (instr-dri-imm instr) (arithmetic-shift instr -16))

(define (instr-i-imm instr) (arithmetic-shift instr -8))

(define (decode-instruction/op-reg-imm/reg instr)
    #f)

(define (decode-instruction/op-reg-reg-imm/reg instr)
    #f)

(define (decode-instruction/op-reg-imm instr)
    #f)

(define (decode-instruction/op-imm instr)
    #f)

(define (decode-instruction instr shape)
    (case shape
        [(op-reg-imm/reg) (decode-instruction/op-reg-imm/reg instr)]
        [(op-reg-reg-imm/reg) (decode-instruction/op-reg-reg-imm/reg instr)]
        [(op-reg-imm) (decode-instruction/op-reg-imm instr)]
        [(op-imm) (decode-instruction/op-imm instr)]))

#|
(define-pb-opcode
    [pb-nop]
    [pb-literal]
    [pb-mov16 pb-keeps pb-shifts]
    [pb-mov pb-move-types]
    [pb-bin-op pb-signals pb-binaries pb-argument-types]
    [pb-cmp-op pb-compares pb-argument-types]
    [pb-fp-bin-op pb-binaries pb-argument-types]
    [pb-un-op pb-unaries pb-argument-types]
    [pb-fp-un-op pb-unaries pb-argument-types]
    [pb-fp-cmp-op pb-compares pb-argument-types]
    [pb-rev-op pb-sizes pb-argument-types]
    [pb-ld-op pb-sizes pb-argument-types]
    [pb-st-op pb-sizes pb-argument-types]
    [pb-b-op pb-branches pb-argument-types]
    [pb-b*-op pb-argument-types]
    [pb-call]
    [pb-return]
    [pb-interp]
    [pb-adr]
    [pb-inc pb-argument-types]
    [pb-lock]
    [pb-cas]
    [pb-call-arena-in] [pb-call-arena-out]
    [pb-fp-call-arena-in] [pb-fp-call-arena-out]
    [pb-stack-call]
    [pb-fence pb-fences]
    [pb-chunk])|#

(define (pb-print-skeleton-instr instr)
	(let ([instr (bytes->instr instr 'little)])
		(format "(opcode: ~a ...)" (instr-op instr))))

(define (bytes->instr-little-endian instr-bytes)
	(bitwise-ior 
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 3) 24) #xFF000000)
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 2) 16) #xFF0000)
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 1) 8) #xFF00)
		(bytes-ref instr-bytes 0)))

(define (bytes->instr-big-endian instr-bytes)
    (bitwise-ior 
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 0) 24) #xFF000000)
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 1) 16) #xFF0000)
		(bitwise-and (arithmetic-shift (bytes-ref instr-bytes 2) 8) #xFF00)
		(bytes-ref instr-bytes 3)))

(define (bytes->instr instr-bytes endian)
	(case endian
		[(little) (bytes->instr-little-endian instr-bytes)]
		[(big) (bytes->instr-big-endian instr-bytes)]))

(define-syntax (in-range stx)
    (syntax-case stx ()
        [(_ x a b)
            #'(and (<= a x) (<= x b))]))

(define (disassemble instr-bytes)
    (let ([instr (bytes->instr instr-bytes 'little)])
        (cond
            [(> (instr-op instr) 255) ]
            [(in-range (instr-op instr) pb-mov-16-group-start (+ pb-mov-16-group-start (sub1 pb-mov-16-group-count)))
                (decode/pb-mov16 instr)]
            [(in-range (instr-op instr) pb-mov-group-start (+ pb-mov-group-start (enum-field-count pb-mov-types)))
                (decode/pb-mov instr)]
            [(in-range (instr-op instr) pb-binop-group-start (+ pb-binop-group-start 
                (* (enum-field-count pb-argument-types) (enum-field-count pb-binaries) (enum-field-count pb-signal-types))))
                (decode/pb-binop instr)]
            [(in-range (instr-op instr) pb-cmp-group-start (+ pb-cmp-group-start 
                (* (enum-field-count pb-argument-types) (enum-field-count pb-cmp-ops))))
                (decode/pb-cmp instr)]
            [(in-range (instr-op instr) pb-fp-binop-group-start (+ pb-fp-binop-group-start 
                (* (enum-field-count pb-argument-types) (enum-field-count pb-binaries))))
                (decode/pb-fp-binop instr)]
            [else (pb-print-skeleton-instr instr-bytes)])))

(define (pb-skeleton-disassemble bs) 
	(let loop ([i 0]) 
		(cond
			[(equal? i (bytes-length bs)) (void)]
			[(<= (+ i pb-instruction-byte-size) (bytes-length bs))
                    (begin
                        (display 
                            (format "~a: ~a" i (disassemble (subbytes bs i (+ i pb-instruction-byte-size)))))
                        (display "\n")
                        (loop (+ i pb-instruction-byte-size)))]
			[else (error 'pb-disassemble "bad instruction format")])))

(define (pb-count-instrs bs)
	(unless (equal? (remainder (bytes-length bs) pb-instruction-byte-size) 0)
		(error 'pb-disassemble "bad instruction format"))
	(quotient (bytes-length bs) pb-instruction-byte-size))

(define (pb-disassemble bs)
	(unless (bytes? bs)
		(error 'pb-disassemble "unexpected input type"))
	(display (string-append
		(format "byte-length: ~a\n" (bytes-length bs))
		(format "number of instructions: ~a\n" (pb-count-instrs bs))))
	(display (pb-skeleton-disassemble bs)))