#lang racket

(require (for-syntax racket/syntax))
(require racket/fixnum)

(provide pb-disassemble pb-config)

(define pb-instruction-byte-size 4)

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
(define pb-literal 1)
(define pb-return 213)
(define pb-adr 215)

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

;; Helper Macro for defining a simple enumeration datatype.
;; Given a `enum-name` and field_1, ... field_n, defines the following:
;; A struct,
;;      (struct <enum-name>-struct [enum-fields])
;;          where enum-fields is a list of symbols representing the fields
;; An instance of the struct called <enum-name>
;; A constant named each of field_1, ..., field_n for easy access to each of the enum fields
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

;; Returns the field names for a corresponding enum by accessing the `enum-fields`
;; of the corresponding instance
(define-syntax (enum-fields stx)
  (syntax-case stx ()
    [(_ enum-name)
     (with-syntax ([accessor-name (format-id stx "~a-struct-enum-fields" (syntax-e #'enum-name))])
       #'(accessor-name enum-name))]))

(define-syntax (enum-field-count stx)
  (syntax-case stx ()
    [(_ enum-name)
        #`(length (enum-fields enum-name))]))

;; PB instructions come in groups of different variants. For example, binops are defined as:
;; [pb-bin-op pb-signals pb-binaries pb-argument-types] 
;; where pb-signals is an enum representing each possible signal, pb-binaries is an enum representing each binop,
;; and pb-argument-types represents immediate or register variants. Taking the product of these options
;; yields the different binop variants. 

;; On the disassembler end, we can use the following macro, deconstruct-op, which is given an opcode and
;; a list of known enum options that have been multiplied to form the opcode, and we can repeatedly
;; take the modulus of the opcode and number of enum fields and "divide away" the number of enum fields
;; in order to figure out exactly which variant an opcode corresponds to.

;; In the binop example, there are two pb-signal variants: true and false. Thus, after subtracting 
;; off a base value, to determine whether an opcode signals or not we can simply take the opcode mod 2.
;; Then, we can floor divide by 2 to collapse each signal variant to a single opcode, and we can continue this 
;; mod/floor divide process for all the other listed enum variants to extract the other variant information.

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
                "r15"))

(define fp-reg-names (vector
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

(define (format-reg r [fp #f])
    (let ([reg-name (if fp (vector-ref fp-reg-names r) (vector-ref reg-names r))])
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

(define (format-instr/dri op-name dest reg imm props [is-fp? '(#f #f #f)])
    (if (null? props)
        (format "(~a ~a ~a ~a)" op-name (format-reg dest (first is-fp?)) (format-reg reg (second is-fp?)) (format-imm imm 0 #f))
        (format "(~a ~a ~a ~a ~a)" op-name (format-reg dest (first is-fp?)) (format-reg reg (second is-fp?)) (format-imm imm 0 #f) (format-props props))))

(define (format-instr/dir op-name dest reg imm props [is-fp? '(#f #f #f)])
    (if (null? props)
        (format "(~a ~a ~a ~a)" op-name (format-reg dest (first is-fp?)) (format-imm imm 0 #f) (format-reg reg (second is-fp?)))
        (format "(~a ~a ~a ~a ~a)" op-name (format-reg dest (first is-fp?)) (format-imm imm 0 #f) (format-reg reg (second is-fp?)) (format-props props))))

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

(define (format-instr/dr op dst reg props [is-fp? '(#f #f)])
    (if (null? props)
        (format "(~a ~a ~a)"
            op
            (format-reg dst (first is-fp?))
            (format-reg reg (second is-fp?)))
        (format "(~a ~a ~a ~a)"
            op
            (format-reg dst (first is-fp?))
            (format-reg reg (second is-fp?))
            (format-props props))))

(define (format-instr/drr op dst r1 r2 props [is-fp? '(#f #f #f)])
    (if (null? props)
        (format "(~a ~a ~a ~a)"
            op
            (format-reg dst (first is-fp?))
            (format-reg r1 (second is-fp?))
            (format-reg r2 (third is-fp?)))
        (format "(~a ~a ~a ~a ~a)"
            op
            (format-reg dst (first is-fp?))
            (format-reg r1 (second is-fp?))
            (format-reg r2 (third is-fp?))
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
        "mov-16"
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
            [else (format-instr/dr "mov" dst reg (list (mov-type movt)))])))

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
    (deconstruct-op (instr-op instr) pb-fp-binop-group-start
        [drr/dri pb-argument-types]
        [op-kind pb-binaries]
        (begin
            (cond 
                [(equal? drr/dri pb-register)
                    (format-instr/drr
                        (vector-ref pb-fp-binop-names op-kind)
                        (instr-drr-dest instr)
                        (instr-drr-reg1 instr)
                        (instr-drr-reg2 instr)
                        '() 
                        '(#t #t #t))]
                [else (error 'pb-disassemble "floating-point instruction cannot have dri variant")]))))

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

(define (decode/pb-fp-cmp-op instr)
    (deconstruct-op (instr-op instr) pb-fp-cmp-op-group-start 
        [drr/dri pb-argument-types]
        [op-kind pb-cmp-ops]
        (cond
            [(equal? drr/dri pb-register)
                (format-instr/dr
                    (vector-ref 
                        pb-fp-cmp-op-names
                        op-kind)
                    (instr-dr-dest instr)
                    (instr-dr-reg instr)
                    '()
                    '(#t #t))]
            [else (error 'pb-disassemble "floating point instruction cannot have dri variant")])))

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
        (let ([fp (or (equal? sz pb-single) (equal? sz pb-double))])
            (cond
                [(equal? drr/dri pb-register)
                    (format-instr/drr
                        (format "ld-~a" (vector-ref pb-size-names sz))
                        (instr-drr-dest instr)
                        (instr-drr-reg1 instr)
                        (instr-drr-reg2 instr)
                        '()
                        (list #f #f))]
                [(equal? drr/dri pb-immediate)
                    (format-instr/dri
                        (format "ld-~a" (vector-ref pb-size-names sz))
                        (instr-dri-dest instr)
                        (instr-dri-reg instr)
                        (instr-dri-imm instr)
                        '()
                        (list fp #f #f))]))))

(define (decode/pb-st-op instr)
    (deconstruct-op (instr-op instr) pb-st-group-start
        [drr/dri pb-argument-types]
        [sz pb-sizes]
        (let 
            ([fp (or (equal? sz pb-double) (equal? sz pb-single))])
            (cond
                [(equal? drr/dri pb-register)
                    (format-instr/drr
                        (format "st-~a" (vector-ref pb-size-names sz))
                        (instr-drr-dest instr)
                        (instr-drr-reg1 instr)
                        (instr-drr-reg2 instr)
                        '()
                        (list fp #f #f))]
                [(equal? drr/dri pb-immediate)
                    (format-instr/dir
                        (format "st-~a" (vector-ref pb-size-names sz))
                        (instr-dri-dest instr)
                        (instr-dri-reg instr)
                        (instr-dri-imm instr)
                        '()
                        (list fp #f #f))]))))

(define (bsearch cmp vec e start end)
  (if (< start end)
      (let*
          ([mid (quotient (+ end start) 2)]
           [cur (vector-ref vec mid)]
           [c (cmp e cur)])
        (cond
          [(equal? c 0) cur]
          [(equal? c -1) (bsearch cmp vec e start mid)]
          [(equal? c 1) (bsearch cmp vec e (+ 1 mid) end)]))
      #f))

(define (cmp-label-offset offset lbl)
    (cond
        [(equal? offset (label-offset lbl)) 0]
        [(< offset (label-offset lbl)) -1]
        [else 1]))

(define (decode/pb-b-op instr i labels)
    (deconstruct-op (instr-op instr) pb-b-group-start
        [r/i pb-argument-types]
        [b-type pb-branches]
        (cond
            [(equal? r/i pb-register)
                (format-instr/d
                    (vector-ref pb-branch-names b-type)
                    (instr-dr-reg instr))]
            [(equal? r/i pb-immediate)
                (let* ([target (+ (* pb-instruction-byte-size (+ 1 i)) (get-branch-target instr))]
                       [label (bsearch cmp-label-offset labels target 0 (vector-length labels))])
                       (format-instr/i
                            (vector-ref pb-branch-names b-type)
                            (instr-i-imm instr) (if label (label-name label) "") 24 #t))])))

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

(define (decode/pb-adr-op instr)
    (format-instr/di
        "adr"
        (instr-adr-dest instr)
        ; immediate for pb-adr is an instruction-level offset
        (* (instr-adr-imm instr) pb-instruction-byte-size)
        '()))

(define decode/pb-nop
    "(nop)")

(define (decode/pb-literal-op instr num-words)
    (format-instr/d
        "literal"
        (instr-di-dest instr)))

(define (decode/pb-return-op instr)
    "(return)")

; (define (decode/pb-call-op instr)
;     "(call)")

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


#|
In Chez Scheme, the rp-header and rp-compact-header structures are defined as follows:

(define-primitive-structure-disps rp-header type-untyped
  ([uptr toplink]
   [uptr mv-return-address]
   [ptr livemask]
   [iptr frame-size]))

(define-primitive-structure-disps rp-compact-header type-untyped
  ([uptr toplink]
   [iptr mask+size+mode]))

Assuming that sizeof(uptr) = sizeof(ptr) = sizeof(iptr) = machine word size,
We have sizeof(rp-header) as 4*(word size). Similarly, sizeof(rp-compact-header) = 2*(word size)
|#


(define size-rp-compact-header 2)
(define size-rp-header 4)

(define (format-label lbl-n)
    (format "l~a" lbl-n))

(struct label ([offset] [name]) #:transparent)
(define (new-label offset n)
    (label offset (format-label n)))

(define (read-instr bs i endian)
    (let ([instr-bytes (subbytes bs i (+ i pb-instruction-byte-size))])
        (bytes->instr instr-bytes endian)))

(define (sort-dedup-labels labels)
    (let* ([sorted (sort labels (lambda (a b) (< a b)))]
           [deduped (remove-duplicates sorted)])
           (reverse (foldl (lambda (offset lst) (cons (new-label offset (length lst)) lst))  '() deduped))))

; note: adapted from chez scheme pbchunk.ss
(define (collect-headers-labels bs config len)
  (define word-size (native-word-size (pb-config-bits config)))
  (let loop ([i 0] [headers '()] [labels '()])
    (cond
      [(fx= i len) (values '() (sort-dedup-labels labels))]

      [(and (pair? headers)
            ; if we hit the index of an rp header, skip over it
            (fx= i (caar headers)))
       (let ([size (cdar headers)])
         (let ([i (+ i size)])
           (let-values ([(rest-headers labels) (loop i (cdr headers) labels)])
             (values (cons (car headers) rest-headers)
                     labels))))]
      [else
       (let ([instr (read-instr bs i (pb-config-endian config))])
         (define (next)
           (loop (fx+ i pb-instruction-byte-size) headers labels))

         (define (next/add-label new-label)
           (loop (fx+ i pb-instruction-byte-size) headers (cons new-label labels)))

         (define (next/adr)
           (let ([delta (fx* pb-instruction-byte-size (instr-adr-imm instr))])
             (cond
               [(> delta 0)
                (let* ([after (fx+ i pb-instruction-byte-size delta)]
                       [size (if (fx= 1 (fxand 1 (bytes-ref bs (fx- after
                                                                            (if (eq? (pb-config-endian config) 'little)
                                                                                word-size
                                                                                1)))))
                                 (* size-rp-compact-header word-size)
                                 (* size-rp-header word-size))]
                       [start (fx- after size)]
                       [header (cons start size)])
                  (loop (fx+ i pb-instruction-byte-size)
                        ;; (mflatt) insert keeping headers sorted
                        (let sort-loop ([headers headers])
                          (cond
                            [(null? headers) (list header)]
                            [(fx<= start (caar headers)) (cons header headers)]
                            [else (cons (car headers) (sort-loop (cdr headers)))]))
                        labels))]
               [else (next)])))

         (define (next-branch)
           (let* ([delta (get-branch-target instr)]
                  [target-label (fx+ i pb-instruction-byte-size delta)])
             (next/add-label target-label)))

         (define (next/literal)
           (loop (fx+ i pb-instruction-byte-size word-size) headers labels))

         (cond
            [(is-branch-imm? instr) (next-branch)]
            [(equal? (instr-op instr) pb-adr) (next/adr)]
            [(equal? (instr-op instr) pb-literal) (next/literal)]
            [else (next)]))])))

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
    (for-each-instr (instr-idx instr) (in-instr-bytes instrs 'little)
        (when (is-branch-imm? instr)
            (let* ([offset (quotient (get-branch-target instr) pb-instruction-byte-size)]
                    [target (+ instr-idx offset)])
                (when (in-range target 0 (vector-length branch-targets))
                    (vector-set! branch-targets target 1)))))
    branch-targets)

(define-syntax (for-each-instr stx)
  (syntax-case stx (in-instr-bytes)
    [(_ (idx instr) (in-instr-bytes bs endian) body ...)
       #'(let loop ([i 0]) 
         (cond
           [(equal? i (bytes-length bs)) (void)]
           [(<= (+ i pb-instruction-byte-size) (bytes-length bs))
            (begin
              (define instr-bytes (subbytes bs i (+ i pb-instruction-byte-size)))
              (define instr (bytes->instr instr-bytes endian))
              (define idx (quotient i pb-instruction-byte-size))
              (define inc (if (equal? (instr-op instr) pb-literal) 3 1))
              
              body ...
              (loop (+ i (* inc pb-instruction-byte-size))))]))]
         [else (error 'pb-disassemble "bad instruction format")]))

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
            #'(and (<= a x) (< x b))]))

(define (pb-print-skeleton-instr instr)
		(format "(opcode: ~a ...)" (instr-op instr)))

(define (literal-word-size sz)
    (cond
        [(equal? sz '32) 2]
        [(equal? sz '64) 3]
        [else (error 'pb-dissassemble "invalid word size ~a" sz)]))

(define (native-word-size sz)
    (cond
        [(equal? sz '32) 4]
        [(equal? sz '64) 8]
        [else (error 'pb-dissassemble "invalid word size ~a" sz)]))

; TODO: Clean up range checking logic here?
(define (disassemble instr instr-idx labels config)
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
            [(in-range (instr-op instr) pb-fp-cmp-op-group-start (+ pb-fp-cmp-op-group-start (* (enum-field-count pb-cmp-ops) (enum-field-count pb-argument-types))))
                (decode/pb-fp-cmp-op instr)]
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
            [(equal? (instr-op instr) pb-literal) (decode/pb-literal-op instr 
                                                        (literal-word-size (pb-config-bits config)))]
            [(equal? (instr-op instr) pb-return) (decode/pb-return-op instr)]
            [(equal? (instr-op instr) pb-adr) (decode/pb-adr-op instr)]
            [else (pb-print-skeleton-instr instr)]))

(struct pb-config ([bits] [endian] [threaded?]) #:transparent)

(define (format-relocation name)
    (format "(relocation ~a)" name))

(define (disassemble-loop bs config relocs) 
    (define word-size (native-word-size (pb-config-bits config)))
    (let-values ([(rp-headers labels) (collect-headers-labels bs config (bytes-length bs))])
        (let*
              ([labels-vec (list->vector labels)]
               [instr-length (pb-count-instrs bs)])
            (let loop ([i 0] [remaining-labels labels] [rs (reverse relocs)] [rps rp-headers]) 
                (cond
                [(equal? i (bytes-length bs)) (void)]
                [(<= (+ i pb-instruction-byte-size) (bytes-length bs))
                    (let* 
                        ([instr (read-instr bs i (pb-config-endian config))]
                         [idx (quotient i pb-instruction-byte-size)]
                         [is-label? (and (pair? remaining-labels)
                                        (equal? i (label-offset (car remaining-labels))))]
                         [is-reloc? (and (pair? rs) (equal? i (+ (cdr (first rs)) 12)))]
                         [is-rp-header? (and (pair? rps)
                                             (equal? i (caar rps)))])
                         (if is-rp-header?
                                (display 
                                    (format "~a:\t rp-header\n" i))
                                (display 
                                    (format "~a:\t ~a\n" i 
                                                        (disassemble instr idx labels-vec config))))
                        
                        ;(display (format "label-offset ~a\n" (label-offset (car remaining-labels))))
                        (when is-label? 
                            (display (format "\n\t.~a:\n" (label-name (car remaining-labels)))))
                        
                        (when is-reloc? 
                            (display (format "~a:\t ~a\n" i (format-relocation (car (first rs))))))
                        
                        (let ([skip (cond
                                        [(equal? (instr-op instr) pb-literal) word-size]
                                        [is-rp-header? (- (cdar rps) pb-instruction-byte-size)]
                                        [else 0])])
                            (loop (+ i pb-instruction-byte-size skip)
                                (if is-label? (cdr remaining-labels) remaining-labels)
                                (if is-reloc? (cdr rs) rs)
                                (if is-rp-header? (cdr rps) rps))))])))))

                ; ; handle the case where a relocation point is at the address after the final instruction 
                ; (unless (null? rs)
                ;     (display (format "~a:\t ~a\n" (* instr-length pb-instruction-byte-size) 
                ;                                                 (format-relocation (car (first rs)))))))))
                
(define (pb-count-instrs bs)
	(unless (equal? (remainder (bytes-length bs) pb-instruction-byte-size) 0)
		(error 'pb-disassemble "bad instruction format"))
	(quotient (bytes-length bs) pb-instruction-byte-size))

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

(define (pb-disassemble bs config relocations)
    (display (format "pb config: ~a" config))
	(unless (bytes? bs)
		(error 'pb-disassemble "unexpected input type"))
	(display (string-append
		(format "byte-length: ~a\n" (bytes-length bs))
		(format "number of instructions: ~a\n" (pb-count-instrs bs))))
    (let-values ([(headers labels) (collect-headers-labels bs config (bytes-length bs))])
        (display headers)
        (display labels)
	    (disassemble-loop bs config relocations)))

; TODO: Fill in other instruction variants