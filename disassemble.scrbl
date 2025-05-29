#lang scribble/manual
(require (for-label racket/base
                      racket/contract
                      ffi/unsafe
                      disassemble))

@title{Disassembler Package Documentation}

@section{Introduction}
This document describes a Racket package for disassembling JIT-compiled Racket functions. It allows you to inspect the underlying machine code generated for your Racket procedures. The package primarily supports x86 and x86-64 architectures. Key functionalities include the `disassemble` procedure for viewing assembly and the `dump` procedure for writing raw machine code to a file. It can optionally use `nasm` for disassembly if available.

@section{API Reference}

@defproc[(disassemble [f procedure?]
                      [#:program prog (or/c #f 'nasm) #f]
                      [#:arch arch (or/c #f symbol?) #f])
         void?]{

  Prints the disassembly of the JIT-compiled Racket procedure @racket[f] to standard output.

  Use @racket[#:program 'nasm] to attempt to use the `ndisasm` utility for disassembly.
  If @racket[prog] is @racket[#f] (the default), an internal disassembler is used.

  Use @racket[#:arch arch-sym] to specify the architecture (e.g., @racket['x86_64], @racket['i386], @racket['aarch64]).
  If @racket[arch-sym] is @racket[#f] (the default), the architecture is auto-detected.

  Example:
  @racketblock[
  (require disassemble)

  (define (my-func x)
    (* x (+ x 2)))

  (disassemble my-func)
  ]
}

This is an alias for the @racket[disassemble] procedure. See the documentation for @racket[disassemble] for details.

@defproc[(dump [f procedure?]
               [file-name path-string?])
         void?]{

  Writes the raw machine code bytes of the JIT-compiled Racket procedure @racket[f]
  to the file specified by @racket[file-name]. If the file already exists,
  it is replaced.

  Example:
  @racketblock[
  (require disassemble)

  (define (another-func y)
    (let ([z (* y y)])
      (/ z 3)))

  (dump another-func "another-func.bin")
  ]
}

@defproc[(disassemble-bytes [bs bytes?]
                            [#:arch arch (or/c #f symbol?) #f]
                            [#:program prog (or/c #f 'nasm) #f]
                            [#:relocations relocations list? '()])
         void?]{

  Prints the disassembly of the raw machine code provided in @racket[bs] to standard output.

  The keyword arguments @racket[#:arch] and @racket[#:program] behave the same as for
  the @racket[disassemble] function.

  The @racket[#:relocations] argument takes a list of pairs, where each pair is
  @racket[(cons <object> <offset>)], to provide context for relocatable addresses
  during disassembly. This is primarily for internal use or advanced scenarios.
}

@defproc[(disassemble-ffi-function [fptr cpointer?]
                                   [#:size s exact-nonnegative-integer?]
                                   [#:program prog (or/c #f 'nasm) #f]
                                   [#:arch arch (or/c #f symbol?) #f])
         void?]{

  Prints the disassembly of machine code located at the C pointer @racket[fptr].
  The @racket[#:size] argument @racket[s] specifies the number of bytes to disassemble.

  The keyword arguments @racket[#:arch] and @racket[#:program] behave the same as for
  the @racket[disassemble] function.

  Note: @racket[cpointer?] is a generic type for C pointers. Ensure the provided
  pointer is valid and points to executable code.
}

@defproc[(get-code-bytes [f procedure?])
         bytes?]{

  Returns the raw machine code bytes for the JIT-compiled Racket procedure @racket[f].
  This is the same sequence of bytes that @racket[dump] would write to a file
  or @racket[disassemble] would process.
}
