#lang racket

(require racket/require ffi/unsafe (subtract-in '#%foreign ffi/unsafe)) 

(define (f x) (* 3 x))

(define _scheme_type _short)
(define _mz_hash_key _short)

(define-cstruct _scheme_object
  ([typetag _scheme_type]
   [key _mz_hash_key]))

(define _scheme_closure_data _gcpointer)

(define-cstruct _scheme_closure 
  ([so _scheme] 
   [code _scheme_closure_data]
   [vals _scheme]))

(define-cstruct _scheme_native_closure 
  ([so _scheme] 
   [code _scheme_closure_data]
   [vals _scheme]))

(define (decompile f)
  (displayln f)
  (define fp (cast f _scheme _scheme_object))
  (scheme_object-typetag fp))

(decompile f)