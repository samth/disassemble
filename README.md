[![Build Status](https://api.travis-ci.org/samth/disassemble.svg)](https://travis-ci.org/samth/disassemble)

A disassembler for JITed functions in Racket.

To install:

    % raco pkg install disassemble

To use it, try something like this:

```
[samth@punge:~/sw/disassemble (master) plt] racket
Welcome to Racket v6.0.1.10.
> (require disassemble)
> (define (f x) 1)
> (disassemble f)
       0: 488943f8                       (mov (mem64+ rbx #x-8) rax)
       4: 4883c3f8                       (add rbx #xfffffffffffffff8)
       8: b803000000                     (mov eax #x3)
       d: 4c8b75c8                       (mov r14 (mem64+ rbp #x-38))
      11: 4883c428                       (add rsp #x28)
      15: 415d                           (pop r13)
      17: 415c                           (pop r12)
      19: 5b                             (pop rbx)
      1a: 5d                             (pop rbp)
      1b: c3                             (ret)
>
```

If you have `ndisasm` installed (and in your `PATH`) you can also try:

```
> (disassemble f #:program 'nasm)
00000000  488943F8          mov [rbx-0x8],rax
00000004  4883C3F8          add rbx,byte -0x8
00000008  B803000000        mov eax,0x3
0000000D  4C8B75C8          mov r14,[rbp-0x38]
00000011  4883C428          add rsp,byte +0x28
00000015  415D              pop r13
00000017  415C              pop r12
00000019  5B                pop rbx
0000001A  5D                pop rbp
0000001B  C3                ret
```

This works only on x86 or x86-64.

Also, the `dump` function writes the bytes of the machine code to a
file:

```
> (dump const "file.bin")
```

Patches, uses, complaints, and suggestions are all welcome.

The disassembly code (when not using NASM) is taken from Göran
Weinholt's [_Industria_ library](http://github.com/weinholt/industria).
