#lang racket

(require racket/require racket/unsafe/ops ffi/unsafe (subtract-in '#%foreign ffi/unsafe)
         "nasm.rkt" racket/flonum) 
#;
(define z 100)
#;
(define f (let ([cnt 0]) (lambda (x y z) (set! cnt (random 100)) (* 3 x cnt z))))

#;
(length 
 (for/list ([i (in-range 100)])
   (set! z (random 1000)) 
   (f 1 2 4)))

(define _mz_hash_key _short)
(define _mzshort _int)

(define _scheme_type 
  (_enum 
   '(prim_type = 27 
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

(define (typeof v) (scheme_object-typetag (cast v _pointer _scheme_object-pointer)))

(define (decompile f #:size [size 128] [env? #f])  
  (define fp (cast f _scheme _scheme_native_closure-pointer))
  (unless (eq? 'native_closure_type (scheme_object-typetag fp))
    (error 'wrong-type))
  (match (scheme_native_closure-code fp)
    [(native_closure_data iso code u arity-code max-let-depth closure-size name retained)
     ;; true if not-yet jitted
     (when (ptr-equal? code on_demand_jit_code)
       (error 'not-yet-jitted))
     (let* ([case? (< closure-size 0)]
            [closure-size (if case?
                              (- (add1 closure-size))
                              closure-size)]
            [tail-code (if case? #f u)]
            [num-arities (if case? closure-size #f)]
            [arities (cast u _gcpointer (_cpointer _mzshort))]
            [env (scheme_native_closure-vals fp)])
       (display (and tail-code (nasm-disassemble (cast tail-code _pointer (_bytes o size)))))
       #;
       (list 
        (list 'name name
              'iso iso 
              'case? case?
              'code code
              'arity-code arity-code
              'max-let-depth max-let-depth
              'closure-size closure-size
              'tail-code tail-code
              'num-arities num-arities 
              'arities arities
              'retained retained)        
        (and env?
             (> closure-size 0)
             (typeof (ptr-ref env _scheme)))))]))
#|
(decompile f)
(define x (case-lambda [(x) 1] [(x y) (list x y)]))
(define (y [x 1] [y 2] [z 3] [w 4] [a 5] [b 6]) 1)
(decompile x)
(decompile y)
|#
#|
(define (id x) 
  (for/fold ([z 0.0]) ([i (in-range 100)])
    (unsafe-fl+ x z)))

(void (id 2.0))

(void (decompile id))
|#
(provide decompile)