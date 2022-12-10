#lang racket

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
       (let loop ([fields* (syntax->datum #'(fields ...))] [i 0])
         (cond
           [(null? fields*) (void)]
           [else
            (with-syntax ([const-name (datum->syntax stx (string->symbol (format "~a-~a" (syntax->datum #'name) (car fields*))))])
              #`(begin
                  (define const-name #,i)
                  #,(loop (cdr fields*) (+ 1 i))))]))]))

(define pb-binop-group-start 22)

(define pb-mov-16-group-start 2)
(define pb-mov-16-group-count 8)

(define/enum pb-shift
    pb-shift0
    pb-shift1
    pb-shift2
    pb-shift3
)

(define/enum pb
    zero-bits
    keep-bits)

(define/enum pb-regs
    tc
    sfp
    ap
    trap
    ac0
    xp
    ts
    td
    cp
    r9
    r10
    r11
    r12
    r13
    r14
    r15
    fp1
    fp2
    fp3
    fp4
    fp5
    fp6
    fp7
    fp8)

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

(define (format-instr/dr-i op-name reg imm properties)
    (format "(~a ~a ~a ~a)" op-name (format-reg reg) (format-imm imm) properties))

(define (format-instr/di op dst imm props)
    (format "(~a ~a ~a ~a)" 
        op
        (format-reg dst)
        (format-imm imm)
        (string-join (map format-prop props))))

(struct zero/keep [zk])
(struct shift [s])

(define (format-prop prop)
    (match prop
        [(zero/keep zk) (format "#:zk ~a" zk)]
        [(shift s) (format "#:shift ~a" s)]))

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
            [(in-range (instr-op instr) pb-mov-16-group-start (+ pb-mov-16-group-start (sub1 pb-mov-16-group-count)))
                (decode/pb-mov16 instr)]
            [else (pb-print-skeleton-instr instr-bytes)])))

(define (pb-skeleton-disassemble bs) 
	(let loop ([i 0]) 
		(cond
			[(equal? i (bytes-length bs)) (void)]
			[(<= (+ i pb-instruction-byte-size) (bytes-length bs))
                    (begin
                        (display 
                            (disassemble (subbytes bs i (+ i pb-instruction-byte-size))))
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
