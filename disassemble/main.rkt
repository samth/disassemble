#lang racket/base

(require racket/match ffi/unsafe racket/lazy-require
         version/utils
         (prefix-in fc: "fcdisasm.rkt")
         (prefix-in x86: "x86.rkt"))

(lazy-require ("nasm.rkt" [nasm-disassemble]))

(provide dump typeof disassemble disassemble-ffi-function (rename-out [disassemble decompile]))

(define _mz_hash_key _short)
(define _mzshort _int)


;; this struct is just to help get the start tag for procedures
(define-cstruct _test_scheme_object
  ([typetag _short]
   [key _mz_hash_key]))

(define (get-tag-num v)
  (test_scheme_object-typetag 
   (cast v _scheme _test_scheme_object-pointer)))

(define prim-type-number (get-tag-num values))

(define _scheme_type
  (_enum ;; from stypes.h
   `(prim_type = ,prim-type-number
     closed_prim_type
     closure_type
     case_closure_type
     cont_type
     escaping_cont_type
     proc_struct_type
     native_closure_type
     proc_chaperone_type)
   _short))

(define-cstruct _scheme_object
  ([typetag _scheme_type]
   [key _mz_hash_key]))

;; we assume that we're always in precise-gc mode
(define _scheme_inclhash_object _scheme_object)

(define-cstruct _scheme_lambda
  ([iso _scheme_inclhash_object]
   [num_params _mzshort]
   [max_let_depth _mzshort]
   [closure_size _mzshort]
   [closure_map (_cpointer _mzshort)]
   [body _scheme]
   [name _scheme]
   [tl_map _gcpointer]
   ;; more fields here for JIT
   ))

(define-cstruct _scheme_closure
  ([so _scheme_object]
   [code _scheme_lambda-pointer]
   [vals _scheme]))

(define-cstruct _native_lambda
  ([iso _scheme_inclhash_object]
   [start_code _fpointer]
   ;; either a void * tail_code (non-case-lambda) or mzshort * arities (case-lambda)
   [u _gcpointer]
   [arity_code _gcpointer]
   [max_let_depth _mzshort]
   [closure_size _mzshort]
   ;; either a
   ;; struct Scheme_Lambda *orig_code; /* For not-yet-JITted non-case-lambda */ or
   ;; Scheme_Object *name;
   [name _scheme]
   [tl_map _gcpointer]
   ;; a void**
   [retained _gcpointer]))

(define-cstruct _scheme_native_closure
  ([so _scheme_object]
   [code _native_lambda-pointer]
   [vals _pointer]))

(define find_jit_code_end (get-ffi-obj "scheme_jit_find_code_end" #f
                                       (_fun _gcpointer -> _gcpointer)))

(define jit-now! (get-ffi-obj "scheme_jit_now" #f (_fun _racket -> _void)))


(define (typeof v) (scheme_object-typetag (cast v _pointer _scheme_object-pointer)))

(define (go name f #:size [size #f])
  (unless (procedure? f)
    (raise-argument-error name "procedure" f))
  (jit-now! f)
  (define fp (cast f _scheme _scheme_native_closure-pointer))
  (unless (eq? 'native_closure_type (typeof fp))
    (raise-argument-error name "non-primitive procedure" f))
  (match (scheme_native_closure-code fp)
    [(native_lambda iso code u arity-code max-let-depth closure-size nm tl_map retained)
     (let* ([case? (< closure-size 0)]
            [closure-size (if case?
                              (sub1 (- closure-size))
                              closure-size)]
            [tail-code (if case? #f u)]
            [num-arities (if case? closure-size #f)]
            [arities (cast u _gcpointer (_cpointer _mzshort))]
            [env (scheme_native_closure-vals fp)]
            [end (and tail-code (find_jit_code_end tail-code))]
            [end (and end (cast end _gcpointer _size))]
            [size (if end (- end
                             (cast tail-code _gcpointer _size))
                      size)])
       (when case?
         (error name "functions defined with `case-lambda' are not yet supported"))
       (unless tail-code
         (error name "unable to read jitted code"))
       (unless (or end size)
         (error name
                "unable to find the end of the jitted code, and no #:size supplied"))
       (cast tail-code _pointer (_bytes o size)))]))

(define systype (if (fixnum? (expt 2 61)) 64 32)) ;; no 16-bit racket
(define color #f) ;; doesn't work currently

(define (disassemble f #:program [prog #f])
  (define bs (go 'disassemble f))
  (case prog
    [(nasm) (display (nasm-disassemble bs))]
    [else
     (fc:disassemble (open-input-bytes bs)
                     (λ (p c) (x86:get-instruction p systype c))
                     color #f 0 '())]))

(define (disassemble-ffi-function fptr #:size s #:program [prog #f])
  (define bs (cast fptr _pointer (_bytes o s)))
  (case prog
    [(nasm) (display (nasm-disassemble bs))]
    [else
     (fc:disassemble (open-input-bytes bs)
                     (λ (p c) (x86:get-instruction p systype c))
                     color #f 0 '())]))

(provide get-code-bytes)
(define (get-code-bytes f) (go 'get-code-bytes f))

(define (dump f file-name)
  (define bs (go 'dump f))
  (let ((file (open-output-file file-name #:exists 'replace)))
    (write-bytes bs file)
    (close-output-port file)))
