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
                        [instance-name (format-id stx "~a" (syntax-e #'name))]
                        [count-name (format-id stx "~a-count" (syntax-e #'name))])
            #`(begin
                ;(display (format "name: ~a" (syntax->datum struct-name)))
                (struct struct-name [enum-fields] #:transparent)
                (define instance-name (struct-name '#,field-symbols))
                (define-syntax count-name #,(length (syntax->datum #'(fields ... ))))

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

(define-syntax (deconstruct-op stx)
  (syntax-case stx ()
    [(_ op base [name enum] ... body)
     #`(let
           ([rel (- op base)])
            #,(let loop ([names (syntax->datum #'(name ...))] [enums (syntax->datum #'(enum ...))] [div 1])
               (cond
                 [(null? names) #'body]
                 [else
                      (with-syntax ([name (format-id stx "~a" (car names))]
                                    [enum-name (format-id stx "~a" (car enums))]
                                    [enum-count (format-id stx "~a-count" (car enums))])
                        #`(let ([name (remainder (quotient rel #,div) #,(syntax-local-value #'enum-count))])
                            #,(loop (cdr names) (cdr enums) (* div (syntax-local-value #'enum-count)))))])))]))

(define pb-binop-group-start 22)
(define pb-mov-16-group-start 2)
(define pb-mov-16-group-count 8)
(define pb-mov-group-start 10)
(define pb-cmp-group-start 74)
(define pb-fp-binop-group-start 92)
(define pb-unop-group-start 118)
(define pb-fp-unop-group-start 122)
(define pb-fp-cmp-op-group-start 126)
(define pb-rev-op-group-start 144)
(define pb-ld-group-start 164)
(define pb-st-group-start 184)
(define pb-b-group-start 204)
(define pb-b*-group-start 210)
(define pb-nop 0)

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

(define/enum pb-sizes
    pb-int8
    pb-uint8
    pb-int16
    pb-uint16
    pb-int32
    pb-uint32
    pb-int64
    pb-uint64
    pb-single
    pb-double)

(define pb-size-names
    (vector
        "int8"
        "uint8"
        "int16"
        "uint16"
        "int32"
        "uint32"
        "int64"
        "uint64"
        "single"
        "double"
    ))

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


  (define/enum pb-unaries
    pb-not
    pb-sqrt)

  (define/enum pb-branches
    pb-fals
    pb-true
    pb-always)

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

(define pb-unop-names
    (vector
        "not"
        "sqrt"))

(define pb-fp-unop-names 
    (vector-map (lambda (n) (format "fp-~a" n)) pb-unop-names))

(define pb-branch-names
    (vector
        "bfalse"
        "btrue"
        "b"))

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

(define pb-fp-cmp-op-names
    (vector-map (lambda (n) (format "fp-~a" n)) pb-cmp-names))

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

(define (s-ext imm imm-sz)
    (let ([sign (arithmetic-shift imm (- (sub1 imm-sz)))])
           (+ (- (arithmetic-shift sign imm-sz)) imm)))
        
(define (format-imm imm imm-sz sgn?)
    (if sgn?
        (format "(imm ~a)" (s-ext imm imm-sz))
        (format "(imm #x~x)" imm)))

(define (format-offset off)
    (format "(offset #x~x)" off))

(define (format-props props)
    (string-join (map format-prop props)))

(define (format-instr/dri op-name dest reg imm props)
    (if (null? props)
        (format "(~a ~a ~a ~a)" op-name (format-reg dest) (format-reg reg) (format-imm imm 0 #f))
        (format "(~a ~a ~a ~a ~a)" op-name (format-reg dest) (format-reg reg) (format-imm imm 0 #f) (format-props props))))

(define (format-instr/dir op-name dest reg imm props)
    (if (null? props)
        (format "(~a ~a ~a ~a)" op-name (format-reg dest) (format-imm imm 0 #f) (format-reg reg))
        (format "(~a ~a ~a ~a ~a)" op-name (format-reg dest) (format-imm imm 0 #f) (format-reg reg) (format-props props))))

(define (format-instr/di op dst imm props)
    (if (null? props)
        (format "(~a ~a ~a)" 
            op
            (format-reg dst)
            (format-imm imm 0 #f))
        (format "(~a ~a ~a ~a)" 
            op
            (format-reg dst)
            (format-imm imm 0 #f)
            (format-props props))))

(define (format-instr/dr op dst reg props)
    (if (null? props)
        (format "(~a ~a ~a)"
            op
            (format-reg dst)
            (format-reg reg))
        (format "(~a ~a ~a ~a)"
            op
            (format-reg dst)
            (format-reg reg)
            (format-props props))))

(define (format-instr/drr op dst r1 r2 props)
    (if (null? props)
        (format "(~a ~a ~a ~a)"
            op
            (format-reg dst)
            (format-reg r1)
            (format-reg r2))
        (format "(~a ~a ~a ~a ~a)"
            op
            (format-reg dst)
            (format-reg r1)
            (format-reg r2)
            (format-props props))))

(define (format-instr/d op dest)
    (format "(~a ~a)"
        op
        (format-reg dest)))

(define (as-offset imm)
    (s-ext imm  24))

(define (format-label-imm label imm im-sz sgn?)
    (format "(label ~a ~a)" label (format-imm imm im-sz sgn?)))

(define (format-instr/i op imm label im-sz sgn?)
    (format "(~a ~a)"
                op
                (if (equal? label "")
                    (format-imm imm im-sz sgn?)
                    (format-label-imm label imm im-sz sgn?))))

; (define (format-instr/adr-dest op addr dst r1 imm)
;     (format "(~a ~a ~a ~a ~a)"
;         op
;         (format-offset addr)
;         (format-reg dst)
;         (format-reg reg)
;         (format-imm imm)))

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
        ([op (instr-op instr)])

        (deconstruct-op op pb-binop-group-start 
                [drr/dri pb-argument-types] 
                [op-kind pb-binaries]
                [sig pb-signal-types]
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
                    (list (signal sig)))]))))

(define (decode/pb-fp-unop instr)
    (deconstruct-op (instr-op instr) pb-fp-unop-group-start
        [dr/di pb-argument-types]
        [op-kind pb-unaries]
        (cond
            [(equal? dr/di pb-register)
                (format-instr/dr
                    (vector-ref pb-fp-unop-names (instr-op instr))
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '())]
            [else (error 'pb-disassemble "floating-point instruction cannot have dri variant")])))

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
    (deconstruct-op (instr-op instr) pb-binop-group-start
        [drr/dri pb-argument-types]
        [op-kind pb-binaries]
        (cond 
            [(equal? drr/dri 0)
                (format-instr/drr
                    (vector-ref pb-fp-binop-names op-kind)
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    '())]
            [else (error 'pb-disassemble "floating-point instruction cannot have dri variant")])))

(define (decode/pb-unop instr) 
    (deconstruct-op (instr-op instr) pb-unop-group-start
        [dr/di pb-argument-types]
        [op-kind pb-unaries]
        (cond
            [(equal? dr/di pb-immediate) 
                (format-instr/di
                    (vector-ref pb-unop-names op-kind)
                    (instr-di-dest instr)
                    (instr-di-imm instr)
                    '())]
            [(equal? dr/di pb-register) 
                (format-instr/dr 
                    (vector-ref pb-unop-names op-kind)
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '())])))

(define (decode/pb-fp-cmp instr)
    (deconstruct-op (instr-op instr) pb-fp-cmp-op-group-start 
        [drr/dri pb-argument-types]
        [op-kind pb-cmp-ops]
        (cond
            [(equal? drr/dri pb-register)
                (format-instr/drr
                    (vector-ref 
                        pb-fp-cmp-op-names
                        op-kind)
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    '())]
            [else (error 'pb-disassemble "floating point instruction canot have dri variant")])))

(define (decode/pb-rev-op instr)
    (deconstruct-op (instr-op instr) pb-rev-op-group-start
        [dr/di pb-argument-types]
        [sz pb-sizes]
        (cond
            [(equal? dr/di pb-register)
                (format-instr/dr
                    (format "rev~a" (vector-ref pb-size-names sz))
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '())]
            [else (error 'pb-disassemble "rev instruction canot have di variant")])))

(define (decode/pb-ld-op instr)
    (deconstruct-op (instr-op instr) pb-ld-group-start
        [drr/dri pb-argument-types]
        [sz pb-sizes]
        (cond
            [(equal? drr/dri pb-register)
                (format-instr/drr
                    (format "ld-~a" (vector-ref pb-size-names sz))
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    '())]
            [(equal? drr/dri pb-immediate)
                (format-instr/dri
                    (format "ld-~a" (vector-ref pb-size-names sz))
                    (instr-dri-dest instr)
                    (instr-dri-reg instr)
                    (instr-dri-imm instr)
                    '())])))

(define (decode/pb-st-op instr)
    (deconstruct-op (instr-op instr) pb-st-group-start
        [drr/dri pb-argument-types]
        [sz pb-sizes]
        (cond
            [(equal? drr/dri pb-register)
                (format-instr/drr
                    (format "st-~a" (vector-ref pb-size-names sz))
                    (instr-drr-dest instr)
                    (instr-drr-reg1 instr)
                    (instr-drr-reg2 instr)
                    '())]
            [(equal? drr/dri pb-immediate)
                (format-instr/dir
                    (format "st-~a" (vector-ref pb-size-names sz))
                    (instr-dri-dest instr)
                    (instr-dri-reg instr)
                    (instr-dri-imm instr)
                    '())])))

(define (decode/pb-b-op instr i labels)
    (deconstruct-op (instr-op instr) pb-b-group-start
        [r/i pb-argument-types]
        [b-type pb-branches]
        (cond
            [(equal? r/i pb-register)
                (format-instr/d
                    (vector-ref pb-branch-names b-type)
                    (instr-d-dest instr))]
            [(equal? r/i pb-immediate)
                (let* ([target  (+ i (quotient (get-branch-target instr) 4))]
                       [label (if (in-range target 0 (sub1 (vector-length labels)))
                                (vector-ref labels target)
                                "")])
                    (format-instr/i
                        (vector-ref pb-branch-names b-type)
                        (instr-i-imm instr) label 24 #t))])))

(define (decode/pb-b*-op instr)
    (deconstruct-op (instr-op instr) pb-b*-group-start
        [dr/di pb-argument-types]
        (cond
            [(equal? dr/di pb-register)
                (format-instr/dr
                    "b*" 
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '())]
            [(equal? dr/di pb-immediate)
                (format-instr/di
                    "b*"
                    (instr-di-dest instr)
                    (instr-di-imm instr)
                    '())])))

(define decode/pb-nop
    "(nop)")

(define (instr-op instr) (bitwise-and instr #xFF))

(define (instr-d-dest instr) (bitwise-and (arithmetic-shift instr -8) #xF))

(define (instr-dr-dest instr) (instr-d-dest instr))
(define (instr-dr-reg instr) (bitwise-and (arithmetic-shift instr -16) #xF))

(define (instr-di-dest instr) (instr-d-dest instr))
(define (instr-di-imm instr) (arithmetic-shift instr -16))

;                     7    0 
; | imm         | r1  | op |
(define (instr-adr-dest instr) (instr-di-dest instr))
(define (instr-adr-imm instr) (arithmetic-shift instr -12))

(define (instr-drr-dest instr) (instr-d-dest instr))
(define (instr-drr-reg1 instr) (bitwise-and (arithmetic-shift instr -12) #xF))
(define (instr-drr-reg2 instr) (bitwise-and (arithmetic-shift instr -16) #xF))

(define (instr-dri-dest instr) (instr-d-dest instr))
(define (instr-dri-reg instr) (bitwise-and (arithmetic-shift instr -12) #xF))
(define (instr-dri-imm instr) (arithmetic-shift instr -16))

(define (instr-i-imm instr) (arithmetic-shift instr -8))


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


(define (is-branch-imm? instr)
    (if (in-range (instr-op instr) pb-b-group-start 
            (+ pb-b-group-start 
            (* (enum-field-count pb-branches) (enum-field-count pb-argument-types))))
       (deconstruct-op (instr-op instr) pb-b-group-start
            [r/i pb-argument-types]
            [_ pb-branches]
            (equal? r/i pb-immediate))
        #f))

(define (get-branch-target b-imm-instr)
    (s-ext (instr-i-imm b-imm-instr) 24))

(define (collect-jump-targets instrs)
    (define branch-targets (make-vector (pb-count-instrs instrs) 0))
    (let loop ([i 0]) 
            (cond
                [(equal? i (bytes-length instrs)) (void)]
                [(<= (+ i pb-instruction-byte-size) (bytes-length instrs))
                    (let* 
                        ([instr-bytes (subbytes instrs i (+ i pb-instruction-byte-size))]
                         [instr-idx (quotient i pb-instruction-byte-size)]
                         [instr (bytes->instr instr-bytes 'little)])
                            (when (is-branch-imm? instr)
                                (let* ([offset (quotient (get-branch-target instr) pb-instruction-byte-size)]
                                       [target (+ instr-idx offset)])
                                    (when (in-range target 0 (vector-length branch-targets))
                                        (begin
                                            (vector-set! branch-targets target 1))))))
                            (loop (+ i pb-instruction-byte-size))]
                [else (error 'pb-disassemble "bad instruction format")]))
    branch-targets)

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

(define (disassemble instr-bytes instr-idx labels)
    (let ([instr (bytes->instr instr-bytes 'little)])
        (cond
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
            [(in-range (instr-op instr) pb-unop-group-start (+ pb-unop-group-start 
                                                                (* (enum-field-count pb-argument-types) (enum-field-count pb-unaries))))
                (decode/pb-unop instr)]
            [(in-range (instr-op instr) pb-fp-unop-group-start (+ pb-fp-unop-group-start (* (enum-field-count pb-argument-types) (enum-field-count pb-unaries))))
                (decode/pb-fp-unop instr)]
            [(in-range (instr-op instr) pb-ld-group-start (+ pb-ld-group-start (* (enum-field-count pb-sizes) (enum-field-count pb-argument-types))))
                (decode/pb-ld-op instr)]
            [(in-range (instr-op instr) pb-st-group-start (+ pb-st-group-start (* (enum-field-count pb-sizes) (enum-field-count pb-argument-types))))
                (decode/pb-st-op instr)]
            [(in-range (instr-op instr) pb-b-group-start (+ pb-b-group-start (* (enum-field-count pb-branches) (enum-field-count pb-argument-types))))
                (decode/pb-b-op instr instr-idx labels)]
            [(in-range (instr-op instr) pb-b*-group-start (+ pb-b*-group-start (enum-field-count pb-argument-types)))
                (decode/pb-b*-op instr)]
            [(equal? (instr-op instr) pb-nop) decode/pb-nop]
            [else (pb-print-skeleton-instr instr-bytes)])))

(define (disassemble-loop bs) 
    (define targets (collect-jump-targets bs))
    (define labels (make-labels targets))
	(let loop ([i 0]) 
		(cond
			[(equal? i (bytes-length bs)) (void)]
			[(<= (+ i pb-instruction-byte-size) (bytes-length bs))
                    (begin
                        (define instr (subbytes bs i (+ i pb-instruction-byte-size)))
                        (define instr-idx (quotient i pb-instruction-byte-size))
                        (define label (vector-ref labels instr-idx))
                        (display 
                            (format "~a:\t ~a\n" i (disassemble instr instr-idx labels)))
                        (unless (equal? label "")
                            (display (format "\t.~a:\n" label)))
                        (loop (+ i pb-instruction-byte-size)))]
			[else (error 'pb-disassemble "bad instruction format")])))

(define (pb-count-instrs bs)
	(unless (equal? (remainder (bytes-length bs) pb-instruction-byte-size) 0)
		(error 'pb-disassemble "bad instruction format"))
	(quotient (bytes-length bs) pb-instruction-byte-size))

(define (format-label lbl-n)
    (format "l~a" lbl-n))

(define (make-labels targets)
    (define labels (make-vector (vector-length targets) ""))
    (let loop ([i 0] [label-count 0])
        (when (< i (vector-length targets))
            (if (equal? (vector-ref targets i) 1)
                (begin 
                    (vector-set! labels i (format-label label-count))
                    (loop (+ i 1) (+ label-count 1)))
                (loop (+ i 1) label-count))))
    labels)

(define (pb-disassemble bs)
	(unless (bytes? bs)
		(error 'pb-disassemble "unexpected input type"))
	(display (string-append
		(format "byte-length: ~a\n" (bytes-length bs))
		(format "number of instructions: ~a\n" (pb-count-instrs bs))))
	(display (disassemble-loop bs)))