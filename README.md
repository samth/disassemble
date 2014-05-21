A disassembler for JITed functions in Racket.

To install:

    % raco pkg install disassemble


To use it, try something like this (`ndisasm` must be in your path):

```
[samth@punge:~/sw/disassemble (master) plt] racket
Welcome to Racket v6.0.1.10.
> (require disassemble)
> (define (const x) 1)
> (disassemble const)
00000000  8943FC            mov [ebx-0x4],eax
00000003  83C3FC            add ebx,byte -0x4
00000006  B803000000        mov eax,0x3
0000000B  83C41C            add esp,byte +0x1c
0000000E  5F                pop edi
0000000F  5E                pop esi
00000010  5B                pop ebx
00000011  5D                pop ebp
00000012  C3                ret
>
```

This works only on x86, and requires a very recent version of Racket.

If you have no `ndisasm`, you can `dump` function to file, and read it 
with another disassembler (e.g. ida pro).
`dump` function additionally takes `file-name` parameter.

Patches, uses, complaints, and suggestions are all welcome.


