#lang racket/base

(require racket/match ffi/unsafe "nasm.rkt")

(provide dump disassemble (rename-out [disassemble decompile]))

(define _mz_hash_key _short)
(define _mzshort _int)

(define _scheme_type
  (_enum
   '(prim_type = 38
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

(define-cstruct _scheme_closure_data
  ([iso _scheme_inclhash_object]
   [num_params _mzshort]
   [max_let_depth _mzshort]
   [closure_size _mzshort]
   [closure_map (_cpointer _mzshort)]
   [code _scheme]
   [name _scheme]
   ;; more fields here for JIT
   ))

(define-cstruct _scheme_closure
  ([so _scheme_object]
   [code _scheme_closure_data-pointer]
   [vals _scheme]))

(define _scheme_closed_prim (_fun _pointer _int (_or-null _scheme_object-pointer) -> _scheme))

(define-cstruct _native_closure_data
  ([iso _scheme_inclhash_object]
   [code _fpointer]
   ;; either a void * tail_code (non-case-lambda) or mzshort * arities (case-lambda)
   [u _gcpointer]
   [arity_code _gcpointer]
   [max_let_depth _mzshort]
   [closure_size _mzshort]
   ;; either a
   ;; struct Scheme_Closure_Data *orig_code; /* For not-yet-JITted non-case-lambda */ or
   ;; Scheme_Object *name;
   [name _scheme]
   ;; a void**
   [retained _gcpointer]))

(define-cstruct _scheme_native_closure
  ([so _scheme_object]
   [code _native_closure_data-pointer]
   [vals _pointer]))

(define on_demand_jit_code (get-ffi-obj "scheme_on_demand_jit_code" #f _pointer))
(define find_jit_code_end (get-ffi-obj "scheme_jit_find_code_end" #f
                                       (_fun _gcpointer -> _gcpointer)))

(define jit-now! (get-ffi-obj "scheme_jit_now" #f (_fun _racket -> _void)))


(define (typeof v) (scheme_object-typetag (cast v _pointer _scheme_object-pointer)))


(define (go name f #:size [size #f] [env? #f])
  (unless (procedure? f)
    (raise-argument-error name "procedure" f))
  (jit-now! f)
  (define fp (cast f _scheme _scheme_native_closure-pointer))
  (unless (eq? 'native_closure_type (scheme_object-typetag fp))
    (raise-argument-error name "non-primitive procedure" f))
  (match (scheme_native_closure-code fp)
    [(native_closure_data iso code u arity-code max-let-depth closure-size nm retained)
     (let* ([case? (< closure-size 0)]
            [closure-size (if case?
                              (- (add1 closure-size))
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
         (error name "unable to find the end of the jitted code, and no #:size supplied"))
       (cast tail-code _pointer (_bytes o size)))]))

(define (disassemble f)
  (define bs (go 'disassemble f))
  (displayln (nasm-disassemble bs)))

(define (dump f file-name)
  (define bs (go 'decompile f))
  (let ((file (open-output-file file-name #:exists 'replace)))
    (write-bytes bs file)
    (close-output-port file)))
