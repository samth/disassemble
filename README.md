A disassembler for JITed functions in Racket.

To install:

    % raco pkg install disassemble


To use it, try something like this (`ndisasm` must be in your path):

```
[samth@punge:~/sw/disassemble (master) plt] racket
Welcome to Racket v5.0.99.6.
> (require disassemble)
> (define (const x) 1)
> (const 3) ;; makes sure that `const' is jitted
1
> (decompile const)
00000000  8943FC            mov [ebx-0x4],eax
00000003  83C3FC            add ebx,byte -0x4
00000006  B803000000        mov eax,0x3
0000000B  83C41C            add esp,byte +0x1c
0000000E  5F                pop edi
0000000F  5E                pop esi
00000010  5B                pop ebx
00000011  5D                pop ebp
00000012  C3                ret
.. much more output ..
```

Currently, `decompile` takes a `#:size` parameter which specifies how
many bytes to decompile, because it doesn't understand x86 code enough
to find the end of the function.

If you have no `ndisasm`, you can `dump` function to file, and read it 
with another disassembler (e.g. ida pro).
`dump` function additionally takes `file-name` parameter.

Patches, uses, complaints, and suggestions are all welcome.


