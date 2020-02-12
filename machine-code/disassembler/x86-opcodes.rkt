;; -*- mode: scheme; coding: utf-8 -*-
;; Opcode table for the Intel 80x86 processor
;; Copyright © 2008, 2009, 2010, 2012, 2013, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Table syntax

;; <table> --> #(<entry>[256])  ; the index is an opcode byte

;; <entry> --> <table>
;;    | <instr>
;;    | <prefix-set>
;;    | <group>
;;    | <datasize>
;;    | <addrsize>
;;    | <mode>
;;    | <sse>
;;    | <vex>
;;    | <mem/reg>
;;    | <f64>
;;    | <d64>
;;    | #f

;; <instr> --> (<mnemonic> <operand>*)

;; <prefix-set> --> (*prefix* <prefix-name>+)

;; <prefix-name> --> operand | address
;;     | cs | ds | es | fs | gs | ss
;;     | lock | repz | repnz
;;     | rex | rex.w | rex.r | rex.x | rex.b

;; <group> --> #(Group <name>
;;                     <reg-vector for ModR/M.mod=0, 1 or 2>
;;                     <reg-vector for ModR/M.mod=3>*)

;; <reg-vector> --> #(<reg-entry>[8])  ; the index is ModR/M.reg

;; <reg-entry> --> #(<entry>[8])  ; the index is ModR/M.R/M
;;    | <instr>
;;    | <datasize>
;;    | <addrsize>
;;    | <mode>
;;    | <sse>
;;    | <vex>
;;    | <f64>
;;    | <d64>
;;    | #f

;; <datasize> --> #(Datasize <entry for 16-bit data>
;;                           <entry for 32-bit data>
;;                           <entry for 64-bit data>)

;; <addrsize> --> #(Datasize <entry for 16-bit addressing>
;;                           <entry for 32-bit addressing>
;;                           <entry for 64-bit addressing>)

;; <mode> --> #(Mode <entry for 16/32-bit mode>
;;                   <entry for 64-bit mode>)

;; <sse> --> #(Prefix <entry for no prefix>
;;                    <entry for F3 prefix>
;;                    <entry for 66 prefix>
;;                    <entry for F2 prefix>)

;; <vex> --> #(VEX <entry for no VEX prefix>
;;                 <entry for VEX.128 prefix>
;;                 <entry for VEX.256 prefix>*)

;; <mem/reg> --> #(Mem/reg <entry for memory operand in ModR/M>
;;                         <entry for register operand>)

;; These can be ignored in legacy mode:
;; <f64> --> #(f64 <instr>)  ; force 64-bit operand size
;; <d64> --> #(d64 <instr>)  ; default 64-bit operand size

;; <name> is a string. <mnemonic> and <operand> are symbols.

;;; Operand syntax

;; The abbreviations here are the same as in the Intel manual,
;; appendix A. Uppercase letters are addressing methods, lower case
;; letters specify operand type. There are some differences though:

;; Normally the operand syntax will have initial upper case letters
;; for designating an addressing method, but where this is not the
;; case a single * has been prepended.

;; "1" has been changed to "*unity", so that all operands are written
;; as symbols.

;; For Intel AVX instructions, the opcode syntaxes K, KW, WK, B, BW,
;; WB, In have been used and are not official.

(library (machine-code disassembler x86-opcodes)
  (export
    opcodes pseudo-mnemonics mnemonic-aliases
    lock-instructions
    branch-hint-instructions
    rep-instructions
    repz-instructions
    bnd-instructions
    XOP-opcode-map-8 XOP-opcode-map-9 XOP-opcode-map-A)
  (import
    (rnrs (6)))

  (define lock-instructions
    '(adc add and btc btr bts cmpxchg cmpxchg8b
          cmpxchg16b dec inc neg not or sbb sub
          xadd xchg xor))

  ;; TODO: Can these use hints? loopnz loopz loop jcxz jecxz jrcxz
  (define branch-hint-instructions
    '(jo jno jb jnb jz jnz jbe jnbe js jns jp jnp jl
         jnl jle jnle))

  (define rep-instructions
    '(ins outs movs lods stos
          ;; VIA PadLock:
          montmul xsha1 xsha256
          xstore xcryptecb
          xcryptcbc xcryptctr
          xcryptcfb xcryptofb))

  (define repz-instructions
    '(cmps scas))

  (define bnd-instructions
    '(call ret jmp jo jno jb jnb jz jnz jbe jnbe js
           jns jp jnp jl jnl jle jnle))

  ;; (mnemonic immediate pseudo-op). This table contains a list of
  ;; pseudo-ops, where `mnemonic' is used in the opcode table,
  ;; `immediate' specifies a value for the immediate operand, and
  ;; `pseudo-op' is a programmer-friendly name. The immediate listed
  ;; is always the last operand in an instruction. Note that some
  ;; instructions have two encodings: one with a register and one with
  ;; an immediate.

  ;; For the *3dnow* mnemonic the rule is that if an immediate is not
  ;; listed here, it is an illegal opcode. For the other mnemonics an
  ;; unlisted immediate normally means the original mnemonic should be
  ;; shown, with immediates preserved.

  ;; Examples:
  ;; (*3dnow* mm0 mm1 #xFF) => invalid opcode
  ;; (*3dnow* mm0 mm1 #x94) <=> (pfmin mm0 mm1)
  ;; (vpermil2ps xmm0 xmm1 xmm3 xmm4 3) <=> (vpermilmo2ps xmm0 xmm1 xmm3 xmm4)
  ;; (vpermil2ps xmm0 xmm1 xmm3 xmm4 15) <=> (vpermil2ps xmm0 xmm1 xmm3 xmm4 15)
  ;; (aam #x0a) <=> (aam)
  (define pseudo-mnemonics
    '((*3dnow* #x0C pi2fw)
      (*3dnow* #x0D pi2fd)
      (*3dnow* #x1C pf2iw)
      (*3dnow* #x1D pf2id)
      (*3dnow* #x80 pfnacc)
      (*3dnow* #x8E pfpnacc)
      (*3dnow* #x90 pfcmpge)
      (*3dnow* #x94 pfmin)

      (*3dnow* #x96 pfrcp)
      (*3dnow* #x97 pfrsqrt)
      (*3dnow* #x9A pfsub)
      (*3dnow* #x9E pfadd)
      (*3dnow* #xA0 pfcmpgt)
      (*3dnow* #xA4 pfmax)
      (*3dnow* #xA6 pfrcpit1)
      (*3dnow* #xA7 pfrsqit1)
      (*3dnow* #xAA pfsubr)
      (*3dnow* #xAA pfacc)
      (*3dnow* #xB0 pfcmpeq)
      (*3dnow* #xB4 pfmul)
      (*3dnow* #xB6 pfrcpit2)
      (*3dnow* #xB7 pmulhrw)
      (*3dnow* #xBB pswapd)
      (*3dnow* #xBF pavgusb)

      ;; These two are from AMD Geode:
      (*3dnow* #x86 pfrcpv)
      (*3dnow* #x87 pfrsqrtv)

      (aam #x0A aam)
      (aad #x0A aad)

      (vpermil2pd #b0000 vpermiltd2pd)
      (vpermil2pd #b0001 vpermiltd2pd)
      (vpermil2pd #b0010 vpermilmo2pd)
      (vpermil2pd #b0011 vpermilmz2pd)

      (vpermil2ps #b0000 vpermiltd2ps)
      (vpermil2ps #b0001 vpermiltd2ps)
      (vpermil2ps #b0010 vpermilmo2ps)
      (vpermil2ps #b0011 vpermilmz2ps)

      (pclmulqdq #b00000000 pclmullqlqdq)
      (pclmulqdq #b00000001 pclmulhqlqdq)
      (pclmulqdq #b00010000 pclmullqhqdq)
      (pclmulqdq #b00010001 pclmulhqhqdq)

      (cmppd 0 cmpeqpd)
      (cmppd 1 cmpltpd)
      (cmppd 2 cmplepd)
      (cmppd 3 cmpunordpd)
      (cmppd 4 cmpneqpd)
      (cmppd 5 cmpnltpd)
      (cmppd 6 cmpnlepd)
      (cmppd 7 cmpordpd)
      (vcmppd #x00 vcmpeqpd)
      (vcmppd #x01 vcmpltpd)
      (vcmppd #x02 vcmplepd)
      (vcmppd #x03 vcmpunordpd)
      (vcmppd #x04 vcmpneqpd)
      (vcmppd #x05 vcmpnltpd)
      (vcmppd #x06 vcmpnlepd)
      (vcmppd #x07 vcmpordpd)
      (vcmppd #x08 vcmpeq_uqpd)
      (vcmppd #x09 vcmpngepd)
      (vcmppd #x0a vcmpngtpd)
      (vcmppd #x0b vcmpfalsepd)
      (vcmppd #x0c vcmpneq_oqpd)
      (vcmppd #x0d vcmpgepd)
      (vcmppd #x0e vcmpgtpd)
      (vcmppd #x0f vcmptruepd)
      (vcmppd #x10 vcmpeq_ospd)
      (vcmppd #x11 vcmplt_oqpd)
      (vcmppd #x12 vcmple_oqpd)
      (vcmppd #x13 vcmpunord_spd)
      (vcmppd #x14 vcmpneq_uspd)
      (vcmppd #x15 vcmpnlt_uqpd)
      (vcmppd #x16 vcmpnle_uqpd)
      (vcmppd #x17 vcmpord_spd)
      (vcmppd #x18 vcmpeq_uspd)
      (vcmppd #x19 vcmpnge_uqpd)
      (vcmppd #x1a vcmpngt_uqpd)
      (vcmppd #x1b vcmpfalse_ospd)
      (vcmppd #x1c vcmpneq_ospd)
      (vcmppd #x1d vcmpge_oqpd)
      (vcmppd #x1e vcmpgt_oqpd)
      (vcmppd #x1f vcmptrue_uspd)

      (cmpps 0 cmpeqps)
      (cmpps 1 cmpltps)
      (cmpps 2 cmpleps)
      (cmpps 3 cmpunordps)
      (cmpps 4 cmpneqps)
      (cmpps 5 cmpnltps)
      (cmpps 6 cmpnleps)
      (cmpps 7 cmpordps)
      (vcmpps #x00 vcmpeqps)
      (vcmpps #x01 vcmpltps)
      (vcmpps #x02 vcmpleps)
      (vcmpps #x03 vcmpunordps)
      (vcmpps #x04 vcmpneqps)
      (vcmpps #x05 vcmpnltps)
      (vcmpps #x06 vcmpnleps)
      (vcmpps #x07 vcmpordps)
      (vcmpps #x08 vcmpeq_uqps)
      (vcmpps #x09 vcmpngeps)
      (vcmpps #x0a vcmpngtps)
      (vcmpps #x0b vcmpfalseps)
      (vcmpps #x0c vcmpneq_oqps)
      (vcmpps #x0d vcmpgeps)
      (vcmpps #x0e vcmpgtps)
      (vcmpps #x0f vcmptrueps)
      (vcmpps #x10 vcmpeq_osps)
      (vcmpps #x11 vcmplt_oqps)
      (vcmpps #x12 vcmple_oqps)
      (vcmpps #x13 vcmpunord_sps)
      (vcmpps #x14 vcmpneq_usps)
      (vcmpps #x15 vcmpnlt_uqps)
      (vcmpps #x16 vcmpnle_uqps)
      (vcmpps #x17 vcmpord_sps)
      (vcmpps #x18 vcmpeq_usps)
      (vcmpps #x19 vcmpnge_uqps)
      (vcmpps #x1a vcmpngt_uqps)
      (vcmpps #x1b vcmpfalse_osps)
      (vcmpps #x1c vcmpneq_osps)
      (vcmpps #x1d vcmpge_oqps)
      (vcmpps #x1e vcmpgt_oqps)
      (vcmpps #x1f vcmptrue_usps)

      (cmpsd 0 cmpeqsd)
      (cmpsd 1 cmpltsd)
      (cmpsd 2 cmplesd)
      (cmpsd 3 cmpunordsd)
      (cmpsd 4 cmpneqsd)
      (cmpsd 5 cmpnltsd)
      (cmpsd 6 cmpnlesd)
      (cmpsd 7 cmpordsd)
      (vcmpsd #x00 vcmpeqsd)
      (vcmpsd #x01 vcmpltsd)
      (vcmpsd #x02 vcmplesd)
      (vcmpsd #x03 vcmpunordsd)
      (vcmpsd #x04 vcmpneqsd)
      (vcmpsd #x05 vcmpnltsd)
      (vcmpsd #x06 vcmpnlesd)
      (vcmpsd #x07 vcmpordsd)
      (vcmpsd #x08 vcmpeq_uqsd)
      (vcmpsd #x09 vcmpngesd)
      (vcmpsd #x0a vcmpngtsd)
      (vcmpsd #x0b vcmpfalsesd)
      (vcmpsd #x0c vcmpneq_oqsd)
      (vcmpsd #x0d vcmpgesd)
      (vcmpsd #x0e vcmpgtsd)
      (vcmpsd #x0f vcmptruesd)
      (vcmpsd #x10 vcmpeq_ossd)
      (vcmpsd #x11 vcmplt_oqsd)
      (vcmpsd #x12 vcmple_oqsd)
      (vcmpsd #x13 vcmpunord_ssd)
      (vcmpsd #x14 vcmpneq_ussd)
      (vcmpsd #x15 vcmpnlt_uqsd)
      (vcmpsd #x16 vcmpnle_uqsd)
      (vcmpsd #x17 vcmpord_ssd)
      (vcmpsd #x18 vcmpeq_ussd)
      (vcmpsd #x19 vcmpnge_uqsd)
      (vcmpsd #x1a vcmpngt_uqsd)
      (vcmpsd #x1b vcmpfalse_ossd)
      (vcmpsd #x1c vcmpneq_ossd)
      (vcmpsd #x1d vcmpge_oqsd)
      (vcmpsd #x1e vcmpgt_oqsd)
      (vcmpsd #x1f vcmptrue_ussd)

      (cmpss 0 cmpeqss)
      (cmpss 1 cmpltss)
      (cmpss 2 cmpless)
      (cmpss 3 cmpunordss)
      (cmpss 4 cmpneqss)
      (cmpss 5 cmpnltss)
      (cmpss 6 cmpnless)
      (cmpss 7 cmpordss)
      (vcmpss #x00 vcmpeqss)
      (vcmpss #x01 vcmpltss)
      (vcmpss #x02 vcmpless)
      (vcmpss #x03 vcmpunordss)
      (vcmpss #x04 vcmpneqss)
      (vcmpss #x05 vcmpnltss)
      (vcmpss #x06 vcmpnless)
      (vcmpss #x07 vcmpordss)
      (vcmpss #x08 vcmpeq_uqss)
      (vcmpss #x09 vcmpngess)
      (vcmpss #x0a vcmpngtss)
      (vcmpss #x0b vcmpfalsess)
      (vcmpss #x0c vcmpneq_oqss)
      (vcmpss #x0d vcmpgess)
      (vcmpss #x0e vcmpgtss)
      (vcmpss #x0f vcmptruess)
      (vcmpss #x10 vcmpeq_osss)
      (vcmpss #x11 vcmplt_oqss)
      (vcmpss #x12 vcmple_oqss)
      (vcmpss #x13 vcmpunord_sss)
      (vcmpss #x14 vcmpneq_usss)
      (vcmpss #x15 vcmpnlt_uqss)
      (vcmpss #x16 vcmpnle_uqss)
      (vcmpss #x17 vcmpord_sss)
      (vcmpss #x18 vcmpeq_usss)
      (vcmpss #x19 vcmpnge_uqss)
      (vcmpss #x1a vcmpngt_uqss)
      (vcmpss #x1b vcmpfalse_osss)
      (vcmpss #x1c vcmpneq_osss)
      (vcmpss #x1d vcmpge_oqss)
      (vcmpss #x1e vcmpgt_oqss)
      (vcmpss #x1f vcmptrue_usss)))

  ;; A mapping from common alternative mnemonics to the mnemonics
  ;; used in the opcodes table.
  (define mnemonic-aliases
    '((wait . fwait)
      (sal . shl)
      (xlat . xlatb)
      (loope . loopz)
      (loopne . loopnz)
      (jnae . jb) (setnae . setb) (cmovnae . cmovb)
      (jae . jnb) (setae . setnb) (cmovae . cmovnb)
      (je . jz) (sete . setz) (cmove . cmovz)
      (jne . jnz) (setne . setnz) (cmovne . cmovnz)
      (jna . jbe) (setna . setbe) (cmovna . cmovbe)
      (ja . jnbe) (seta . setnbe) (cmova . cmovnbe)
      (jpe . jp) (setpe . setp) (cmovpe . cmovp)
      (jpo . jnp) (setpo . setnp) (cmovpo . cmovnp)
      (jnge . jl) (setnge . setl) (cmovnge . cmovl)
      (jge . jnl) (setge . setnl) (cmovge . cmovnl)
      (jng . jle) (setng . setle) (cmovng . cmovle)
      (jg . jnle) (setg . setnle) (cmovg . cmovnle)
      (jc . jb) (setc . setb) (cmovc . cmovb)
      (jnc . jnb) (setnc . setnb) (cmovnc . cmovnb)
      (int1 . icebp)
      (setalc . salc)))

  (define opcodes
    '#((add Eb Gb)
       (add Ev Gv)
       (add Gb Eb)
       (add Gv Ev)
       (add *AL Ib)
       (add *rAX Iz)
       #(Mode (push *ES) #f)
       #(Mode (pop *ES) #f)
       ;; 08
       (or Eb Gb)
       (or Ev Gv)
       (or Gb Eb)
       (or Gv Ev)
       (or *AL Ib)
       (or *rAX Iz)
       #(Mode (push *CS) #f)

       ;; 0F: Two-byte opcodes
       #(#(Group "Group 6"
                 #((sldt Rv/Mw) (str Rv/Mw) (lldt Ew) (ltr Ew)
                   (verr Ew) (verw Ew) (jmpe Ev) #f))
         #(Group "Group 7"
                 #((sgdt Ms) (sidt Ms) (lgdt Ms) (lidt Ms)
                   (smsw Rv/Mw) #f (lmsw Ew) (invlpg Mb))
                 #(#(#f (vmcall) (vmlaunch) (vmresume) (vmxoff)
                        #f #f #f)
                   #((monitor) (mwait) (clac) (stac) #f #f #f #f)
                   #((xgetbv) (xsetbv) #f #f #f #f #f #f)
                   #((vmrun) (vmmcall) (vmload) (vmsave)
                     (stgi) (clgi) (skinit) (invlpga))
                   (smsw Rv/Mw)
                   #f
                   (lmsw Ew)
                   #(#(Mode #f (swapgs)) (rdtscp) #f #f #f #f #f #f)))
         (lar Gv Ew)
         (lsl Gv Ew)
         #f
         #(Mode #f (syscall))
         (clts)
         #(Mode #f #(Datasize (sysret) (sysret) (sysretq)))
         ;; 0F 08
         (invd)
         (wbinvd)
         #f
         (ud2)
         #f
         #(Group "Group P"
                 #((prefetch Mb) (prefetchw Mb)
                   ;; Reserved aliases:
                   (prefetch Mb) (prefetchw Mb)
                   (prefetch Mb) (prefetch Mb)
                   (prefetch Mb) (prefetch Mb)))
         (femms)
         (*3dnow* Pq Qq Ib)
         ;; 0F 10
         #(Prefix #(VEX (movups Vps Wps) (vmovups Vps Wps))
                  #(VEX (movss Vss Wss)
                        #(Mem/reg (vmovss Vss Mq) (vmovss Vss Bss Wss))
                        #f)
                  #(VEX (movupd Vpd Wpd) (vmovupd Vpd Wpd))
                  #(VEX (movsd Vsd Wsd)
                        #(Mem/reg (vmovsd Vsd Mq) (vmovsd Vsd Bsd Wsd))
                        #f))
         #(Prefix #(VEX (movups Wps Vps) (vmovups Wps Vps))
                  #(VEX (movss Wss Vss)
                        #(Mem/reg (vmovss Mq Vss) (vmovss Wss Bss Vss))
                        #f)
                  #(VEX (movupd Wpd Vpd) (vmovupd Wpd Vpd))
                  #(VEX (movsd Wsd Vsd)
                        #(Mem/reg (vmovsd Mq Vsd) (vmovsd Wsd Bsd Vsd))
                        #f))
         #(Prefix #(VEX #(Mem/reg (movlps Vps Mq) (movhlps Vps Uq))
                        #(Mem/reg (vmovlps Vps Bps Mq) (vmovhlps Vps Bps Uq))
                        #f)
                  #(VEX (movsldup Vps Wps) (vmovsldup Vps Wps))
                  #(VEX (movlpd Vsd Mq) (vmovlpd Vsd Bsd Mq))
                  #(VEX (movddup Vpd Wsd) (vmovddup Vpd Wsd)))
         #(Prefix #(VEX (movlps Mq Vps) (vmovlps Mq Vps) #f)
                  #f
                  #(VEX (movlpd Mq Vsd) (vmovlpd Mq Vsd) #f)
                  #f)
         #(Prefix #(VEX (unpcklps Vps Wq) (vunpcklps Vps Bps Wq)) #f
                  #(VEX (unpcklpd Vpd Wq) (vunpcklpd Vpd Bpd Wq)) #f)
         #(Prefix #(VEX (unpckhps Vps Wq) (vunpckhps Vps Bps Wq)) #f
                  #(VEX (unpckhpd Vpd Wq) (vunpckhpd Vpd Bpd Wq)) #f)
         #(Prefix #(VEX #(Mem/reg (movhps Vps Mq) (movlhps Vps Uq))
                        #(Mem/reg (vmovhps Vps Bps Mq) (vmovlhps Vps Bps Uq))
                        #f)
                  #(VEX (movshdup Vps Wps) (vmovshdup Vps Wps))
                  #(VEX (movhpd Vsd Mq) (vmovhpd Vsd Bsd Mq) #f)
                  #f)
         #(Prefix #(VEX (movhps Mq Vps) (vmovhps Mq Vps) #f) #f
                  #(VEX (movhpd Mq Vsd) (vmovhpd Mq Vsd) #f) #f)
         ;; 0F 18
         #(Group "Group 16"
                 #((prefetchnta Mb) (prefetcht0 Mb)
                   (prefetcht1 Mb)  (prefetcht2 Mb)
                   ;; Reserved for future use:
                   (prefetchnta Mb) (prefetchnta Mb)
                   (prefetchnta Mb) (prefetchnta Mb))
                 #(#f #f #f #f #f #f #f #f))
         (nop Ev)
         #(Prefix (bndldx bnd Emib)
                  (bndcl bnd Edq/mode)
                  (bndmov bnd Ebnd)
                  (bndcu bnd Edq/mode))
         #(Prefix (bndstx Emib bnd)
                  (bndmk bnd Edq/mode/norel)
                  (bndmov Ebnd bnd)
                  (bndcn bnd Edq/mode))
         (nop Ev)
         (nop Ev)
         (nop Ev)
         (nop Ev)
         ;; 0F 20
         (mov Rd/q Cd/q)
         (mov Rd/q Dd/q)
         (mov Cd/q Rd/q)
         (mov Dd/q Rd/q)
         #f                           ;(MOV r32,tr). AMD SSE5 was here
         #f
         #f                             ;(MOV tr,r32)
         #f
         ;; 0F 28
         #(Prefix #(VEX (movaps Vps Wps) (vmovaps Vps Wps))
                  #f
                  #(VEX (movapd Vpd Wpd) (vmovapd Vpd Wpd))
                  #f)
         #(Prefix #(VEX (movaps Wps Vps) (vmovaps Vps Wps))
                  #f
                  #(VEX (movapd Wpd Vpd) (vmovapd Wpd Vpd))
                  #f)
         #(Prefix (cvtpi2ps Vps Qq)
                  #(VEX (cvtsi2ss Vss Ed/q)
                        (vcvtsi2ss Vss Bss Ed/q)
                        #f)
                  (cvtpi2pd Vpd Qq)
                  #(VEX (cvtsi2sd Vsd Ed/q)
                        (vcvtsi2sd Vsd Bsd Ed/q)
                        #f))
         #(Prefix #(VEX (movntps Mdq Vps)
                        (vmovntps Mdq Vps)
                        #f)
                  (movntss Md Vss)
                  #(VEX (movntpd Mdq Vpd)
                        (vmovntpd Mdq Vpd)
                        #f)
                  (movntsd Mq Vsd))
         #(Prefix (cvttps2pi Pq Wps)
                  #(VEX (cvttss2si Gd/q Wss)
                        (vcvttss2si Gd/q Wss)
                        #f)
                  (cvttpd2pi Pq Wpd)
                  #(VEX (cvttsd2si Gd/q Wsd)
                        (vcvttsd2si Gd/q Wsd)
                        #f))
         #(Prefix (cvtps2pi Pq Wps)
                  #(VEX (cvtss2si Gd/q Wss)
                        (vcvtss2si Gd/q Wss)
                        #f)
                  (cvtpd2pi Pq Wpd)
                  #(VEX (cvtsd2si Gd/q Wsd)
                        (vcvtsd2si Gd/q Wsd)
                        #f))
         #(Prefix #(VEX (ucomiss Vss Wss) (vucomiss Vss Wss) #f)
                  #f
                  #(VEX (ucomisd Vsd Wsd) (vucomisd Vsd Wsd) #f)
                  #f)
         #(Prefix #(VEX (comiss Vps Wps)
                        (vcomiss Vps Wps)
                        #f)
                  #f
                  #(VEX (comisd Vpd Wsd)
                        (vcomisd Vpd Wsd)
                        #f)
                  #f)
         ;; 0F 30
         (wrmsr)
         (rdtsc)
         (rdmsr)
         (rdpmc)
         #(Mode (sysenter) #f)
         #(Mode (sysexit) #f)
         #f
         (getsec)
         ;; 0F 38: Three-byte opcode
         #(#(Prefix (pshufb Pq Qq) #f #(VEX (pshufb Vdq Wdq) (vpshufb Vdq Bdq Wdq) #f) #f)
           #(Prefix (phaddw Pq Qq) #f #(VEX (phaddw Vdq Wdq) (vphaddw Vdq Bdq Wdq) #f) #f)
           #(Prefix (phaddd Pq Qq) #f #(VEX (phaddd Vdq Wdq) (vphaddd Vdq Bdq Wdq) #f) #f)
           #(Prefix (phaddsw Pq Qq) #f #(VEX (phaddsw Vdq Wdq) (vphaddsw Vdq Bdq Wdq) #f) #f)
           #(Prefix (pmaddubsw Pq Qq) #f #(VEX (pmaddubsw Vdq Wdq) (vpmaddubsw Vdq Bdq Wdq) #f) #f)
           #(Prefix (phsubw Pq Qq) #f #(VEX (phsubw Vdq Wdq) (vphsubw Vdq Bdq Wdq) #f) #f)
           #(Prefix (phsubd Pq Qq) #f #(VEX (phsubd Vdq Wdq) (vphsubd Vdq Bdq Wdq) #f) #f)
           #(Prefix (phsubsw Pq Qq) #f #(VEX (phsubsw Vdq Wdq) (vphsubsw Vdq Bdq Wdq) #f) #f)
           ;; 0F 38 08
           #(Prefix (psignb Pq Qq) #f #(VEX (psignb Vdq Wdq) (vpsignb Vdq Bdq Wdq) #f) #f)
           #(Prefix (psignw Pq Qq) #f #(VEX (psignw Vdq Wdq) (vpsignw Vdq Bdq Wdq) #f) #f)
           #(Prefix (psignd Pq Qq) #f #(VEX (psignd Vdq Wdq) (vpsignd Vdq Bdq Wdq) #f) #f)
           #(Prefix (pmulhrsw Pq Qq) #f #(VEX (pmulhrsw Vdq Wdq) (vpmulhrsw Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX #f (vpermilps Vps Bps Wps)) #f)
           #(Prefix #f #f #(VEX #f (vpermilpd Vpd Bpd Wpd)) #f)
           #(Prefix #f #f #(VEX #f (vtestps Vps Wps)) #f)
           #(Prefix #f #f #(VEX #f (vtestpd Vpd Wpd)) #f)
           ;; 0F 38 10
           #(Prefix #f #f (pblendvb Vdq Wdq) #f)
           #f #f #f
           #(Prefix #f #f (blendvps Vdq Wdq) #f)
           #(Prefix #f #f (blendvpd Vdq Wdq) #f)
           #f
           #(Prefix #f #f #(VEX (ptest Vdq Wdq) (vptest Vdq Wdq)) #f)
           ;; 0F 38 18
           #(Prefix #f #f #(VEX #f (vbroadcastss Vss Md)) #f)
           #(Prefix #f #f #(VEX #f #f (vbroadcastsd Vsd Mq)) #f)
           #(Prefix #f #f #(VEX #f #f (vbroadcastf128 Vsd Mdq)) #f)
           #f
           #(Prefix (pabsb Pq Qq) #f
                    #(VEX (pabsb Vdq Wdq) (vpabsb Vdq Wdq) #f) #f)
           #(Prefix (pabsw Pq Qq) #f
                    #(VEX (pabsw Vdq Wdq) (vpabsw Vdq Wdq) #f) #f)
           #(Prefix (pabsd Pq Qq) #f
                    #(VEX (pabsd Vdq Wdq) (vpabsd Vdq Wdq) #f) #f)
           #f
           ;; 0F 38 20
           #(Prefix #f #f #(VEX (pmovsxbw Vdq Udq/Mq) (vpmovsxbw Vdq Udq/Mq) #f) #f)
           #(Prefix #f #f #(VEX (pmovsxbd Vdq Udq/Md) (vpmovsxbd Vdq Udq/Md) #f) #f)
           #(Prefix #f #f #(VEX (pmovsxbq Vdq Udq/Mw) (vpmovsxbq Vdq Udq/Mw) #f) #f)
           #(Prefix #f #f #(VEX (pmovsxwd Vdq Udq/Md) (vpmovsxwd Vdq Udq/Md) #f) #f)
           #(Prefix #f #f #(VEX (pmovsxwq Vdq Udq/Mq) (vpmovsxwq Vdq Udq/Mq) #f) #f)
           #(Prefix #f #f #(VEX (pmovsxdq Vdq Udq/Mq) (vpmovsxdq Vdq Udq/Mq) #f) #f)
           #f #f
           ;; 0F 38 28
           #(Prefix #f #f #(VEX (pmuldq Vdq Wdq) (vpmuldq Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pcmpeqq Vdq Wdq) (vpcmpeqq Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (movntdqa Vdq Mdq) (vmovntdqa Vdq Mdq) #f) #f)
           #(Prefix #f #f #(VEX (packusdw Vdq Wdq) (vpackusdw Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX #f (vmaskmovps Vdq Bps Mps)) #f)
           #(Prefix #f #f #(VEX #f (vmaskmovpd Vdq Bpd Mpd)) #f)
           #(Prefix #f #f #(VEX #f (vmaskmovps Mps Bdq Vps)) #f)
           #(Prefix #f #f #(VEX #f (vmaskmovpd Mpd Bdq Vpd)) #f)
           ;; 0F 38 30
           #(Prefix #f #f #(VEX (pmovzxbw Vdq Udq/Mq) (vpmovzxbw Vdq Udq/Mq) #f) #f)
           #(Prefix #f #f #(VEX (pmovzxbd Vdq Udq/Md) (vpmovzxbd Vdq Udq/Md) #f) #f)
           #(Prefix #f #f #(VEX (pmovzxbq Vdq Udq/Mw) (vpmovzxbq Vdq Udq/Mw) #f) #f)
           #(Prefix #f #f #(VEX (pmovzxwd Vdq Udq/Mq) (vpmovzxwd Vdq Udq/Mq) #f) #f)
           #(Prefix #f #f #(VEX (pmovzxwq Vdq Udq/Md) (vpmovzxwq Vdq Udq/Md) #f) #f)
           #(Prefix #f #f #(VEX (pmovzxdq Vdq Udq/Mq) (vpmovzxdq Vdq Udq/Mq) #f) #f)
           #f
           #(Prefix #f #f #(VEX (pcmpgtq Vdq Wdq) (vpcmpgtq Vdq Bdq Wdq) #f) #f)
           ;; 0F 38 38
           #(Prefix #f #f #(VEX (pminsb Vdq Wdq) (vpminsb Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pminsd Vdq Wdq) (vpminsd Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pminuw Vdq Wdq) (vpminuw Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pminud Vdq Wdq) (vpminud Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pmaxsb Vdq Wdq) (vpmaxsb Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pmaxsd Vdq Wdq) (vpmaxsd Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pmaxuw Vdq Wdq) (vpmaxuw Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (pmaxud Vdq Wdq) (vpmaxud Vdq Bdq Wdq) #f) #f)
           ;; 0F 38 40
           #(Prefix #f #f #(VEX (pmulld Vdq Wdq) (vpmulld Vdq Bdq Wdq) #f) #f)
           #(Prefix #f #f #(VEX (phminposuw Vdq Wdq) (vphminposuw Vdq Wdq) #f) #f)
           #f #f #f #f #f #f
           ;; 0F 38 48
           #f #f #f #f #f #f #f #f
           ;; 0F 38 50
           #f #f #f #f #f #f #f #f
           ;; 0F 38 58
           #f #f #f #f #f #f #f #f
           ;; 0F 38 60
           #f #f #f #f #f #f #f #f
           ;; 0F 38 68
           #f #f #f #f #f #f #f #f
           ;; 0F 38 70
           #f #f #f #f #f #f #f #f
           ;; 0F 38 78
           #f #f #f #f #f #f #f #f
           ;; 0F 38 80
           #(Prefix #f
                    #f
                    #(Mode (invept Gd Mdq)
                           (invept Gq Mdq))
                    #f)
           #(Prefix #f
                    #f
                    #(Mode (invvpid Gd Mdq)
                           (invvpid Gq Mdq))
                    #f)
           #f #f #f #f #f #f
           ;; 0F 38 88
           #f #f #f #f #f #f #f #f
           ;; 0F 38 90
           #f #f #f #f
           #f #f
           #(Prefix #f #f               ;FMA4 stuff
                    #(VEX #f #(Datasize #f
                                        (vfmaddsub132ps Vps Bdq Wps)
                                        (vfmaddsub132pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsubadd132ps Vps Bdq Wps)
                                        (vfmsubadd132pd Vpd Bdq Wpd)))
                    #f)
           ;; 0F 38 98
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd132ps Vps Bdq Wps)
                                        (vfmadd132pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd132ss Vss Bss Wss)
                                        (vfmadd132sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub132ps Vps Bdq Wps)
                                        (vfmsub132pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub132ss Vss Bss Wss)
                                        (vfmsub132sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd132ps Vps Bdq Wps)
                                        (vfnmadd132pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd132ss Vss Bss Wss)
                                        (vfnmadd132sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub132ps Vps Bdq Wps)
                                        (vfnmsub132pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub132ss Vss Bss Wss)
                                        (vfnmsub132sd Vsd Bsd Wsd)))
                    #f)
           ;; 0F 38 A0
           #f #f #f #f #f #f
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmaddsub213ps Vps Bdq Wps)
                                        (vfmaddsub213pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsubadd213ps Vps Bdq Wps)
                                        (vfmsubadd213pd Vpd Bdq Wpd)))
                    #f)
           ;; 0F 38 A8
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd213ps Vps Bdq Wps)
                                        (vfmadd213pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd213ss Vss Bss Wss)
                                        (vfmadd213sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub213ps Vps Bdq Wps)
                                        (vfmsub213pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub213ss Vss Bss Wss)
                                        (vfmsub213sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd213ps Vps Bdq Wps)
                                        (vfnmadd213pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd213ss Vss Bss Wss)
                                        (vfnmadd213sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub213ps Vps Bdq Wps)
                                        (vfnmsub213pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub213ss Vss Bss Wss)
                                        (vfnmsub213sd Vsd Bsd Wsd)))
                    #f)
           ;; 0F 38 B0
           #f #f #f #f #f #f
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmaddsub231ps Vps Bdq Wps)
                                        (vfmaddsub231pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsubadd231ps Vps Bdq Wps)
                                        (vfmsubadd231pd Vpd Bdq Wpd)))
                    #f)
           ;; 0F 38 B8
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd231ps Vps Bdq Wps)
                                        (vfmadd231pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmadd231ss Vss Bss Wss)
                                        (vfmadd231sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub231ps Vps Bdq Wps)
                                        (vfmsub231pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfmsub231ss Vss Bss Wss)
                                        (vfmsub231sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd231ps Vps Bdq Wps)
                                        (vfnmadd231pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmadd231ss Vss Bss Wss)
                                        (vfnmadd231sd Vsd Bsd Wsd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub231ps Vps Bdq Wps)
                                        (vfnmsub231pd Vpd Bdq Wpd)))
                    #f)
           #(Prefix #f #f
                    #(VEX #f #(Datasize #f
                                        (vfnmsub231ss Vss Bss Wss)
                                        (vfnmsub231sd Vsd Bsd Wsd)))
                    #f)
           ;; 0F 38 C0
           #f #f #f #f #f #f #f #f
           ;; 0F 38 C8
           #(Prefix (sha1nexte Vdq Wdq) #f #f #f)
           #(Prefix (sha1msg1 Vdq Wdq) #f #f #f)
           #(Prefix (sha1msg2 Vdq Wdq) #f #f #f)
           #(Prefix (sha256rnds2 Vdq Wdq *XMM0) #f #f #f)
           #(Prefix (sha256msg1 Vdq Wdq) #f #f #f)
           #(Prefix (sha256msg2 Vdq Wdq) #f #f #f)
           #f #f
           ;; 0F 38 D0
           #f #f #f #f #f #f #f #f
           ;; 0F 38 D8
           #f #f #f
           #(Prefix #f #f #(VEX (aesimc Vdq Wdq) (vaesimc Vo Wo)) #f)
           #(Prefix #f #f #(VEX (aesenc Vdq Wdq) (vaesenc Vo Ho Wo)) #f)
           #(Prefix #f #f #(VEX (aesenclast Vdq Wdq) (vaesenclast Vo Ho Wo)) #f)
           #(Prefix #f #f #(VEX (aesdec Vdq Wdq) (vaesdec Vo Ho Wo)) #f)
           #(Prefix #f #f #(VEX (aesdeclast Vdq Wdq) (vaesdeclast Vo Ho Wo)) #f)
           ;; 0F 38 E0
           #f #f #f #f #f #f #f #f
           ;; 0F 38 E8
           #f #f #f #f #f #f #f #f
           ;; 0F 38 F0
           #(Prefix/eos (movbe Gv Mv)
                        ;; XXX: AMD indicates a VEX encoding of this
                        (crc32 Gd/q Eb))
           #(Prefix/eos (movbe Mv Gv)
                        (crc32 Gd/q Ev))
           #(VEX #f (andn Gd/q By Ed/q))
           #(Group "VEX group #17"
                 #(#f
                   #(VEX #f (blsr By Ed/q))
                   #(VEX #f (blsmsk By Ed/q))
                   #(VEX #f (blsi By Ed/q))
                   #f #f #f #f))
           #f #f
           #(Prefix #f (adox Gd/q Ed/q) (adcx Gd/q Ed/q) #f)
           #(VEX #f (bextr Gd/q Ed/q By))
           ;; 0F 38 F8
           #f #f #f #f #f #f #f #f)
         (dmint)
         ;; 0F 3A: Three-byte opcode (also RDM in AMD Geode)
         #(#f
           #f
           #f
           #f
           #(Prefix #f #f #(VEX #f (vpermilps Vps Wps Ib)) #f)
           #(Prefix #f #f #(VEX #f (vpermilpd Vpd Wpd Ib)) #f)
           #(Prefix #f #f #(VEX #f #f (vperm2f128 Vdq Bdq Wdq Ib)) #f)
           #f
           ;; 0F 3A 08
           #(Prefix #f #f #(VEX (roundps Vdq Wdq Ib) (vroundps Vdq Wdq Ib)) #f)
           #(Prefix #f #f #(VEX (roundpd Vdq Wdq Ib) (vroundpd Vdq Wdq Ib)) #f)
           #(Prefix #f #f #(VEX (roundss Vss Wss Ib) (vroundss Vss Bss Wss Ib) #f) #f)
           #(Prefix #f #f #(VEX (roundsd Vsd Wsd Ib) (vroundsd Vsd Bsd Wsd Ib) #f) #f)
           #(Prefix #f #f #(VEX (blendps Vdq Wdq Ib) (vblendps Vdq Bdq Wdq Ib)) #f)
           #(Prefix #f #f #(VEX (blendpd Vdq Wdq Ib) (vblendpd Vdq Bdq Wdq Ib)) #f)
           #(Prefix #f #f #(VEX (pblendw Vdq Wdq Ib) (vpblendw Vdq Bdq Wdq Ib) #f) #f)
           #(Prefix (palignr Vq Qq Ib) #f
                    #(VEX (palignr Vdq Wdq Ib)
                          (vpalignr Vdq Bdq Wdq Ib)
                          #f)
                    #f)
           ;; 0F 3A 10
           #f #f #f #f
           #(Prefix #f #f #(VEX (pextrb Rd/Mb Vdq Ib) (vpextrb Rd/Mb Vdq Ib) #f) #f)
           #(Prefix #f #f #(VEX (pextrw Rd/Mw Vdq Ib) (vpextrw Rd/Mw Vdq Ib) #f) #f)
           #(Prefix #f
                    #f
                    #(Datasize #f
                               #(VEX (pextrd Ed Vdq Ib) (vpextrd Ed Vdq Ib) #f)
                               #(VEX (pextrq Eq Vdq Ib) (vpextrq Eq Vdq Ib) #f))
                    #f)
           #(Prefix #f #f #(VEX (extractps Ed Vdq Ib)
                                (vextractps Ed Vdq Ib)
                                #f)
                    #f)
           ;; 0F 3A 18
           #(Prefix #f #f #(VEX #f #f (vinsertf128 Vdq Bdq Wo Ib)) #f)
           #(Prefix #f #f #(VEX #f #f (vextractf128 Wo Bdq Ib)) #f)
           #f
           #f
           #f
           #f                           ;TODO: vcvtps2ph
           #f
           #f
           ;; 0F 3A 20
           #(Prefix #f #f
                    #(VEX (pinsrb Vdq Rd/Mb Ib)
                          (vpinsrb Vdq Bdq Rd/Mb Ib)
                          #f)
                    #f)
           #(Prefix #f #f
                    #(VEX (insertps Vdq Udq/Md Ib)
                          (vinsertps Vdq Bdq Udq/Md Ib)
                          #f)
                    #f)
           #(Prefix #f
                    #f
                    #(Datasize #f
                               #(VEX (pinsrd Vdq Ed Ib) (vpinsrd Vdq Bdq Ed Ib) #f)
                               #(VEX (pinsrq Vdq Eq Ib) (vpinsrq Vdq Bdq Eq Ib) #f))
                    #f)
           #f #f #f #f #f
           ;; 0F 3A 28
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 30
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 38
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 40
           #(Prefix #f #f #(VEX (dpps Vps Wps Ib) (vdpps Vps Bps Wpd Ib)) #f)
           #(Prefix #f #f #(VEX (dppd Vpd Wpd Ib) (vdppd Vpd Bpd Wpd Ib) #f) #f)
           #(Prefix #f #f #(VEX (mpsadbw Vdq Wdq Ib) (vmpsadbw Vdq Bdq Wdq Ib) #f) #f)
           #f
           #(Prefix #f #f (pclmulqdq Vdq Wdq Ib) #f)
           #f
           #f
           #f
           ;; 0F 3A 48
           #(Prefix #f #f #(VEX #f (vpermil2ps Vps Bps WKps KWps In)) #f)
           #(Prefix #f #f #(VEX #f (vpermil2pd Vpd Bpd WKpd KWpd In)) #f)
           #(Prefix #f #f #(VEX #f (vblendvps Vx Hx Wx Lx)) #f)
           #(Prefix #f #f #(VEX #f (vblendvpd Vx Hx Wx Lx)) #f)
           #(Prefix #f #f #(VEX #f (vpblendvb Vo Ho Wo Lo)) #f)
           #f
           #f
           #f
           ;; 0F 3A 50
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 58
           #f
           #f
           #f
           #f
           #(Prefix #f #f #(VEX #f (vfmaddsubps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfmaddsubpd Vpd Kpd WBpd BWpd)) #f)
           #(Prefix #f #f #(VEX #f (vfmsubaddps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfmsubaddpd Vpd Kpd WBpd BWpd)) #f)
           ;; 0F 3A 60
           #(Prefix #f #f #(VEX (pcmpestrm Vdq Wdq Ib) (vpcmpestrm Vdq Wdq Ib) #f) #f)
           #(Prefix #f #f #(VEX (pcmpestri Vdq Wdq Ib) (vpcmpestri Vdq Wdq Ib) #f) #f)
           #(Prefix #f #f #(VEX (pcmpistrm Vdq Wdq Ib) (vpcmpistrm Vdq Wdq Ib) #f) #f)
           #(Prefix #f #f #(VEX (pcmpistri Vdq Wdq Ib) (vpcmpistri Vdq Wdq Ib) #f) #f)
           #f #f #f #f
           ;; 0F 3A 68
           #(Prefix #f #f #(VEX #f (vfmaddps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfmaddpd Vpd Kpd WBpd BWpd)) #f)
           #(Prefix #f #f #(VEX #f (vfmaddss Vss Kss WBss BWss) #f) #f)
           #(Prefix #f #f #(VEX #f (vfmaddsd Vsd Ksd WBsd BWsd) #f) #f)
           #(Prefix #f #f #(VEX #f (vfmsubps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfmsubpd Vpd Kpd WBpd BWpd)) #f)
           #(Prefix #f #f #(VEX #f (vfmsubss Vss Kss WBss BWss) #f) #f)
           #(Prefix #f #f #(VEX #f (vfmsubsd Vsd Ksd WBsd BWsd) #f) #f)
           ;; 0F 3A 70
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 78
           #(Prefix #f #f #(VEX #f (vfnmaddps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfnmaddpd Vpd Kpd WBpd BWpd)) #f)
           #(Prefix #f #f #(VEX #f (vfnmaddss Vss Kss WBss BWss) #f) #f)
           #(Prefix #f #f #(VEX #f (vfnmaddsd Vsd Ksd WBsd BWsd) #f) #f)
           #(Prefix #f #f #(VEX #f (vfnmsubps Vps Kps WBps BWps)) #f)
           #(Prefix #f #f #(VEX #f (vfnmsubpd Vpd Kpd WBpd BWpd)) #f)
           #(Prefix #f #f #(VEX #f (vfnmsubss Vss Kss WBss BWss) #f) #f)
           #(Prefix #f #f #(VEX #f (vfnmsubsd Vsd Ksd WBsd BWsd) #f) #f)
           ;; 0F 3A 80
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 88
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 90
           #f #f #f #f #f #f #f #f
           ;; 0F 3A 98
           #f #f #f #f #f #f #f #f
           ;; 0F 3A A0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A A8
           #f #f #f #f #f #f #f #f
           ;; 0F 3A B0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A B8
           #f #f #f #f #f #f #f #f
           ;; 0F 3A C0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A C8
           #f #f #f #f
           #(Prefix (sha1rnds4 Vdq Wdq Ib) #f #f #f)
           #f #f #f
           ;; 0F 3A D0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A D8
           #f #f #f #f #f #f #f
           #(Prefix #f #f
                    #(VEX (aeskeygenassist Vdq Wdq Ib) (vaeskeygenassist Vdq Wo Ib))
                    #f)
           ;; 0F 3A E0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A E8
           #f #f #f #f #f #f #f #f
           ;; 0F 3A F0
           #f #f #f #f #f #f #f #f
           ;; 0F 3A F8
           #f #f #f #f #f #f #f #f)
         #f #f #f #f #f
         ;; 0F 40
         (cmovo Gv Ev)
         (cmovno Gv Ev)
         (cmovb Gv Ev)
         (cmovnb Gv Ev)
         (cmovz Gv Ev)
         (cmovnz Gv Ev)
         (cmovbe Gv Ev)
         (cmovnbe Gv Ev)
         ;; 0F 48
         (cmovs Gv Ev)
         (cmovns Gv Ev)
         (cmovp Gv Ev)
         (cmovnp Gv Ev)
         (cmovl Gv Ev)
         (cmovnl Gv Ev)
         (cmovle Gv Ev)
         (cmovnle Gv Ev)
         ;; 0F 50
         #(Prefix #(VEX (movmskps Gd Ups) (vmovmskps Gd Ups)) #f
                  #(VEX (movmskpd Gd Upd) (vmovmskpd Gd Upd)) #f)
         #(Prefix #(VEX (sqrtps Vps Wps) (vsqrtps Vps Wps))
                  #(VEX (sqrtss Vss Wss) (vsqrtss Vss Bss Wss) #f)
                  #(VEX (sqrtpd Vpd Wpd) (vsqrtpd Vpd Wpd))
                  #(VEX (sqrtsd Vsd Wsd) (vsqrtsd Vsd Bsd Wsd) #f))
         #(Prefix #(VEX (rsqrtps Vps Wps) (vrsqrtps Vps Wps))
                  #(VEX (rsqrtss Vss Wss) (vrsqrtss Vss Bss Wss) #f)
                  #f #f)
         #(Prefix #(VEX (rcpps Vps Wps) (vrcpps Vps Wps))
                  #(VEX (rcpss Vss Wss) (vrcpss Vss Bss Wss) #f)
                  #f #f)
         #(Prefix #(VEX (andps Vps Wps) (vandps Vps Bps Wps)) #f
                  #(VEX (andpd Vpd Wpd) (vandpd Vpd Bpd Wpd)) #f)
         #(Prefix #(VEX (andnps Vps Wps) (vandnps Vpd Bpd Wpd)) #f
                  #(VEX (andnpd Vpd Wpd) (vandnpd Vpd Bpd Wpd)) #f)
         #(Prefix #(VEX (orps Vps Wps) (vorps Vps Bps Wps)) #f
                  #(VEX (orpd Vpd Wpd) (vorpd Vpd Bpd Wpd)) #f)
         #(Prefix #(VEX (xorps Vps Wps) (vxorps Vps Bps Wps)) #f
                  #(VEX (xorpd Vpd Wpd) (vxorpd Vpd Bpd Wpd)) #f)
         ;; 0F 58
         #(Prefix #(VEX (addps Vps Wps)
                        (vaddps Vps Bps Wps))
                  #(VEX (addss Vss Wss)
                        (vaddss Vss Bss Wss)
                        #f)
                  #(VEX (addpd Vpd Wpd)
                        (vaddpd Vpd Bpd Wpd))
                  #(VEX (addsd Vsd Wsd)
                        (vaddsd Vsd Bsd Wsd)
                        #f))
         #(Prefix #(VEX (mulps Vps Wps) (vmulps Vps Bps Wps))
                  #(VEX (mulss Vss Wss) (vmulss Vss Bss Wss) #f)
                  #(VEX (mulpd Vpd Wpd) (vmulpd Vpd Bpd Wpd))
                  #(VEX (mulsd Vsd Wsd) (vmulsd Vsd Bsd Wsd) #f))
         #(Prefix #(VEX (cvtps2pd Vpd Wps)
                        (vcvtps2pd Vpd Wps/128))
                  #(VEX (cvtss2sd Vsd Wss)
                        (vcvtss2sd Vsd Bdq Wss)
                        #f)
                  #(VEX (cvtpd2ps Vps Wpd)
                        (vcvtpd2ps Vps Wpd))
                  #(VEX (cvtsd2ss Vss Wsd)
                        (vcvtsd2ss Vss Bdq Wsd)))
         #(Prefix #(VEX (cvtdq2ps Vps Wdq)
                        (vcvtdq2ps Vps Wdq))
                  #(VEX (cvttps2dq Vdq Wps)
                        (vcvttps2dq Vdq Wps))
                  #(VEX (cvtps2dq Vdq Wps)
                        (vcvtps2dq Vdq Wps))
                  #f)
         #(Prefix #(VEX (subps Vps Wps) (vsubps Vps Bps Wps))
                  #(VEX (subss Vss Wss) (vsubss Vss Bss Wss) #f)
                  #(VEX (subpd Vpd Wpd) (vsubpd Vpd Bpd Wpd))
                  #(VEX (subsd Vsd Wsd) (vsubsd Vsd Bsd Wsd) #f))
         #(Prefix #(VEX (minps Vps Wps) (vminps Vps Bps Wps))
                  #(VEX (minss Vss Wss) (vminss Vss Bss Wss) #f)
                  #(VEX (minpd Vpd Wpd) (vminpd Vpd Bpd Wpd))
                  #(VEX (minsd Vsd Wsd) (vminsd Vsd Bsd Wsd) #f))
         #(Prefix #(VEX (divps Vps Wps) (vdivps Vps Bps Wps))
                  #(VEX (divss Vss Wss) (vdivss Vss Bss Wss) #f)
                  #(VEX (divpd Vpd Wpd) (vdivpd Vpd Bpd Wpd))
                  #(VEX (divsd Vsd Wsd) (vdivsd Vsd Bsd Wsd) #f))
         #(Prefix #(VEX (maxps Vps Wps) (vmaxps Vps Bps Wps))
                  #(VEX (maxss Vss Wss) (vmaxss Vss Bss Wss) #f)
                  #(VEX (maxpd Vpd Wpd) (vmaxpd Vpd Bpd Wpd))
                  #(VEX (maxsd Vsd Wsd) (vmaxsd Vsd Bsd Wpd) #f))
         ;; 0F 60
         #(Prefix (punpcklbw Pq Qd) #f #(VEX (punpcklbw Vdq Wq) (vpunpcklbw Vdq Bdq Wq) #f) #f)
         #(Prefix (punpcklwd Pq Qd) #f #(VEX (punpcklwd Vdq Wq) (vpunpcklwd Vdq Bdq Wq) #f) #f)
         #(Prefix (punpckldq Pq Qd) #f #(VEX (punpckldq Vdq Wq) (vpunpckldq Vdq Bdq Wq) #f) #f)
         #(Prefix (packsswb Pq Qq) #f #(VEX (packsswb Vdq Wdq) (vpacksswb Vdq Bdq Wdq) #f) #f)
         #(Prefix (pcmpgtb Pq Qq) #f #(VEX (pcmpgtb Vdq Wdq) (vpcmpgtb Vdq Bdq Wdq) #f) #f)
         #(Prefix (pcmpgtw Pq Qq) #f #(VEX (pcmpgtw Vdq Wdq) (vpcmpgtw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pcmpgtd Pq Qq) #f #(VEX (pcmpgtd Vdq Wdq) (vpcmpgtd Vdq Bdq Wdq) #f) #f)
         #(Prefix (packuswb Pq Qq) #f #(VEX (packuswb Vdq Wdq) (vpackuswb Vdq Wdq) #f) #f)
         ;; 0F 68
         #(Prefix (punpckhbw Pq Qd) #f #(VEX (punpckhbw Vdq Wq) (vpunpckhbw Vdq Bdq Wq) #f) #f)
         #(Prefix (punpckhwd Pq Qd) #f #(VEX (punpckhwd Vdq Wq) (vpunpckhwd Vdq Bdq Wq) #f) #f)
         #(Prefix (punpckhdq Pq Qd) #f #(VEX (punpckhdq Vdq Wq) (vpunpckhdq Vdq Bdq Wq) #f) #f)
         #(Prefix (packssdw Pq Qq) #f #(VEX (packssdw Vdq Wdq) (vpackssdw Vdq Bdq Wdq) #f) #f)
         #(Prefix #f #f #(VEX (punpcklqdq Vdq Wq) (vpunpcklqdq Vdq Bdq Wq) #f) #f)
         #(Prefix #f #f #(VEX (punpckhqdq Vdq Wq) (vpunpckhqdq Vdq Bdq Wq) #f) #f)
         #(Prefix #(Datasize #f
                             (movd Pq Ed)
                             (movq Pq Eq))
                  #f
                  #(Datasize #f
                             #(VEX (movd Vdq Ed) (vmovd Vdq Ed) #f)
                             #(VEX (movq Vdq Eq) (vmovq Vdq Eq) #f))
                  #f)
         #(Prefix (movq Pq Qq)
                  #(VEX (movdqu Vdq Wdq) (vmovdqu Vdq Wdq))
                  #(VEX (movdqa Vdq Wdq) (vmovdqa Vdq Wdq))
                  #f)
         ;; 0F 70
         #(Prefix (pshufw Pq Qq Ib)
                  #(VEX (pshufhw Vdq Wdq Ib) (vpshufhw Vdq Wdq Ib) #f)
                  #(VEX (pshufd Vdq Wdq Ib) (vpshufd Vdq Wdq Ib) #f)
                  #(VEX (pshuflw Vdq Wdq Ib) (vpshuflw Vdq Wdq Ib) #f))
         #(Group "Group 12"
                 #(#f #f #f #f #f #f #f #f)
                 #(#f #f
                      #(Prefix (psrlw Nq Ib) #f #(VEX (psrlw Udq Ib) (vpsrlw Bdq Udq Ib) #f) #f) #f
                      #(Prefix (psraw Nq Ib) #f #(VEX (psraw Udq Ib) (vpsraw Bdq Udq Ib) #f) #f) #f
                      #(Prefix (psllw Nq Ib) #f #(VEX (psllw Udq Ib) (vpsllw Bdq Udq Ib) #f) #f) #f))
         #(Group "Group 13"
                 #(#f #f #f #f #f #f #f #f)
                 #(#f #f
                      #(Prefix (psrld Nq Ib) #f #(VEX (psrld Udq Ib) (vpsrld Bdq Udq Ib) #f) #f) #f
                      #(Prefix (psrad Nq Ib) #f #(VEX (psrad Udq Ib) (vpsrad Bdq Udq Ib) #f) #f) #f
                      #(Prefix (pslld Nq Ib) #f #(VEX (pslld Udq Ib) (vpslld Bdq Udq Ib) #f) #f) #f))
         #(Group "Group 14"
                 #(#f #f #f #f #f #f #f #f)
                 #(#f #f
                      #(Prefix (psrlq Nq Ib) #f #(VEX (psrlq Udq Ib) (vpsrlq Bdq Udq Ib) #f) #f)
                      #(Prefix #f            #f #(VEX (psrldq Udq Ib) (vpsrldq Bdq Udq Ib) #f) #f)
                      #f #f
                      #(Prefix (psllq Nq Ib) #f #(VEX (psllq Udq Ib) (vpsllq Bdq Udq Ib) #f) #f)
                      #(Prefix #f #f #(VEX (pslldq Udq Ib) (vpslldq Bdq Udq Ib) #f) #f)))
         #(Prefix (pcmpeqb Pq Qq) #f #(VEX (pcmpeqb Vdq Wdq) (vpcmpeqb Vdq Bdq Wdq) #f) #f)
         #(Prefix (pcmpeqw Pq Qq) #f #(VEX (pcmpeqw Vdq Wdq) (vpcmpeqw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pcmpeqd Pq Qq) #f #(VEX (pcmpeqd Vdq Wdq) (vpcmpeqd Vdq Bdq Wdq) #f) #f)
         #(Prefix #(VEX (emms) (vzeroupper) (vzeroall)) #f #f #f)
         ;; 0F 78
         #(Prefix #(Mode (vmread Ed Gd) ;SVDC sr, m80 on AMD Geode
                         (vmread Eq Gq))
                  #f
                  #(Group "Group 17"
                          #((extrq Vdq Ib Ib)
                            #f #f #f #f #f #f #f))
                  (insertq Vdq Uq Ib Ib))
         #(Prefix #(Mode (vmwrite Gd Ed) ;RSDC sr, m80 on AMD Geode
                         (vmwrite Gq Eq))
                  #f
                  (extrq Vdq Uq)
                  (insertq Vdq Udq))
         ;; 0F 7A: AMD SSE5 (also SVLDT m80 on AMD Geode)
         #(#f #f #f #f #f #f #f #f
           ;; 0F 7A 08
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 10
           (frczps Vps Wps)
           (frczpd Vpd Wpd)
           (frczss Vss Wss)
           (frczsd Vsd Wsd)
           #f #f #f #f
           ;; 0F 7A 18
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 20
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 28
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 30
           (cvtph2ps Vps Wdq)
           (cvtps2ph Wdq Vps)
           #f #f #f #f #f #f
           ;; 0F 7A 38
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 40
           #f
           (phaddbw Vdq Wdq)
           (phaddbd Vdq Wdq)
           (phaddbq Vdq Wdq)
           #f #f
           (phaddwd Vdq Wdq)
           (phaddwq Vdq Wdq)
           ;; 0F 7A 48
           #f #f #f
           (phadddq Vdq Wdq)
           #f #f #f #f
           ;; 0F 7A 50
           #f
           (phaddubw Vdq Wdq)
           (phaddubd Vdq Wdq)
           (phaddubq Vdq Wdq)
           #f #f
           (phadduwd Vdq Wdq)
           (phadduwq Vdq Wdq)
           ;; 0F 7A 58
           #f #f #f
           (phaddudq Vdq Wdq)
           #f #f #f #f
           ;; 0F 7A 60
           #f
           (phsubbw Vdq Wdq)
           (phsubwd Vdq Wdq)
           (phsubdq Vdq Wdq)
           #f #f #f #f
           ;; 0F 7A 68
           #f #f #f #f #f #f #f #f
           ;; 0F 7A 70
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A 80
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A 90
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A A0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A B0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A C0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A D0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A E0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
           ;; 0F 7A F0
           #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
         ;; AMD SSE5 (RSLDT m80 on AMD Geode)
         #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f
              ;; 0F 7B 40
              (protb Vpd Wdq Ib)
              (protw Vpd Wdq Ib)
              (protd Vpd Wdq Ib)
              (protq Vpd Wdq Ib)
              #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
              #f #f #f #f #f #f #f #f #f #f)
         #(Prefix (svts Mem80)
                  #f
                  #(VEX (haddpd Vpd Wpd) (vhaddpd Vpd Bpd Wpd))
                  #(VEX (haddps Vps Wps) (vhaddps Vps Bps Wps)))
         #(Prefix (rsts Mem80)
                  #f
                  #(VEX (hsubpd Vpd Wpd) (vhsubpd Vpd Bpd Wpd))
                  #(VEX (hsubps Vps Wps) (vhsubps Vps Bps Wps)))
         #(Prefix #(Datasize #f
                             (movd Ed Pq)
                             (movq Eq Pq))
                  #(VEX (movq Vq Wq) (vmovq Vq Wq) #f)
                  #(Datasize #f
                             #(VEX (movd Ed Vdq) (vmovd Ed Vdq) #f)
                             #(VEX (movq Eq Vdq) (vmovq Eq Vdq) #f))
                  #f)
         #(Prefix (movq Qq Pq)
                  #(VEX (movdqu Wdq Vdq) (vmovdqu Wdq Vdq))
                  #(VEX (movdqa Wdq Vdq) (vmovdqa Wdq Vdq))
                  #f)
         ;; 0F 80
         #(f64 (jo Jz))
         #(f64 (jno Jz))
         #(f64 (jb Jz))
         #(f64 (jnb Jz))
         #(f64 (jz Jz))
         #(f64 (jnz Jz))
         #(f64 (jbe Jz))
         #(f64 (jnbe Jz))
         ;; 0F 88
         #(f64 (js Jz))
         #(f64 (jns Jz))
         #(f64 (jp Jz))
         #(f64 (jnp Jz))
         #(f64 (jl Jz))
         #(f64 (jnl Jz))
         #(f64 (jle Jz))
         #(f64 (jnle Jz))
         ;; 0F 90
         (seto Eb)
         (setno Eb)
         (setb Eb)
         (setnb Eb)
         (setz Eb)
         (setnz Eb)
         (setbe Eb)
         (setnbe Eb)
         ;; 0F 98
         (sets Eb)
         (setns Eb)
         (setp Eb)
         (setnp Eb)
         (setl Eb)
         (setnl Eb)
         (setle Eb)
         (setnle Eb)
         ;; 0F A0
         #(d64 (push *FS))
         #(d64 (pop *FS))
         (cpuid)
         (bt Ev Gv)
         (shld Ev Gv Ib)
         (shld Ev Gv *CL)
         #(Group "VIA PadLock Group"
                 #(#f #f #f #f #f #f #f #f)
                 #((montmul) (xsha1) (xsha256)
                   #f #f #f #f #f))
         #(Group "VIA PadLock Group"
                 #(#f #f #f #f #f #f #f #f)
                 #((xstore) (xcryptecb)
                   (xcryptcbc) (xcryptctr)
                   (xcryptcfb) (xcryptofb)
                   #f #f))
         ;; 0F A8
         #(d64 (push *GS))
         #(d64 (pop *GS))
         (rsm)
         (bts Ev Gv)
         (shrd Ev Gv Ib)
         (shrd Ev Gv *CL)
         #(Group "Group 15"
                 #((fxsave M) (fxrstor M)
                   #(VEX (ldmxcsr Md) (vldmxcsr Md) #f)
                   #(VEX (stmxcsr Md) (vstmxcsr Md) #f)
                   (xsave M) (xrstor M) #f (clflush Mb))
                 #(#f #f #f #f #f (lfence) (mfence) (sfence)))
         (imul Gv Ev)
         ;; 0F B0
         (cmpxchg Eb Gb)
         (cmpxchg Ev Gv)
         (lss Gv Mp)
         (btr Ev Gv)
         (lfs Gv Mp)
         (lgs Gv Mp)
         (movzx Gv Eb)
         (movzx Gv Ew)
         ;; 0F B8
         #(Prefix (jmpe Jz) (popcnt Gv Ev) #f #f)
         #(Group "Group 10"
                 #(#f #f #f #f #f #f #f #f))
         #(Group "Group 8"
                 #(#f #f #f #f
                      (bt Ev Ib) (bts Ev Ib)
                      (btr Ev Ib) (btc Ev Ib)))
         (btc Ev Gv)
         (bsf Gv Ev)
         #(Prefix (bsr Gv Ev) (lzcnt Gv Ev) #f #f)
         (movsx Gv Eb)
         (movsx Gv Ew)
         ;; 0F C0
         (xadd Eb Gb)
         (xadd Ev Gv)
         #(Prefix #(VEX (cmpps Vps Wps Ib)
                        (vcmpps Vps Bps Wps Ib))
                  #(VEX (cmpss Vss Wss Ib)
                        (vcmpss Vss Bss Wss Ib)
                        #f)
                  #(VEX (cmppd Vpd Wpd Ib)
                        (vcmppd Vpd Bpd Wpd Ib))
                  #(VEX (cmpsd Vsd Wsd Ib)
                        (vcmpsd Vsd Bsd Wsd Ib)
                        #f))
         (movnti Md/q Gd/q)
         #(Prefix (pinsrw Pq Ew Ib) #f
                  #(VEX (pinsrw Vdq Rd/Mw Ib) (vpinsrw Vdq Bdq Rd/Mw Ib) #f) #f)
         #(Prefix (pextrw Gd Nq Ib) #f
                  #(VEX (pextrw Gd Udq Ib) (vpextrw Gd Udq Ib) #f) #f)
         #(Prefix #(VEX (shufps Vps Wps Ib) (vshufps Vps Bps Wps Ib)) #f
                  #(VEX (shufpd Vpd Wpd Ib) (vshufpd Vpd Bpd Wpd Ib)) #f)
         #(Group "Group 9"
                 #(#f #(Datasize #f
                                 (cmpxchg8b Mq)
                                 (cmpxchg16b Mdq))
                      #f #f #f #f
                      #(Prefix (vmptrld Mq) (vmxon Mq) (vmclear Mq) #f)
                      (vmptrst Mq))
                 #(#f #f #f #f #f #f (rdrand Ev) (rdseed Ev)))
         ;; 0F C8
         (bswap *rAX/r8)
         (bswap *rCX/r9)
         (bswap *rDX/r10)
         (bswap *rBX/r11)
         (bswap *rSP/r12)
         (bswap *rBP/r13)
         (bswap *rSI/r14)
         (bswap *rDI/r15)
         ;; 0F D0
         #(Prefix #f #f
                  #(VEX (addsubpd Vpd Wpd)
                        (vaddsubpd Vpd Bpd Wpd))
                  #(VEX (addsubps Vps Wps)
                        (vaddsubps Vps Bps Wps)))
         #(Prefix (psrlw Pq Qq) #f #(VEX (psrlw Vdq Wdq) (vpsrlw Vdq Bdq Wdq) #f) #f)
         #(Prefix (psrld Pq Qq) #f #(VEX (psrld Vdq Wdq) (vpsrld Vdq Bdq Wdq) #f) #f)
         #(Prefix (psrlq Pq Qq) #f #(VEX (psrlq Vdq Wdq) (vpsrlq Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddq Pq Qq) #f #(VEX (paddq Vdq Wdq) (vpaddq Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmullw Pq Qq) #f #(VEX (pmullw Vdq Wdq) (vpmullw Vdq Bdq Wdq) #f) #f)
         #(Prefix #f
                  (movq2dq Vdq Nq)
                  #(VEX (movq Wq Vq) (vmovq Wq Vq) #f)
                  (movdq2q Pq Nq))
         #(Prefix (pmovmskb Gd Nq) #f #(VEX (pmovmskb Gd Udq) (vpmovmskb Gd Udq) #f) #f)
         ;; 0F D8 (also SMINT)
         #(Prefix (psubusb Pq Qq) #f #(VEX (psubusb Vdq Wdq) (vpsubusb Vdq Bdq Wdq) #f) #f)
         #(Prefix (psubusw Pq Qq) #f #(VEX (psubusw Vdq Wdq) (vpsubusw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pminub Pq Qq) #f #(VEX (pminub Vdq Wdq) (vpminub Vdq Bdq Wdq) #f) #f)
         #(Prefix (pand Pq Qq) #f #(VEX (pand Vdq Wdq) (vpand Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddusb Pq Qq) #f #(VEX (paddusb Vdq Wdq) (vpaddusb Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddusw Pq Qq) #f #(VEX (paddusw Vdq Wdq) (vpaddusw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmaxub Pq Qq) #f #(VEX (pmaxub Vdq Wdq) (vpmaxub Vdq Bdq Wdq) #f) #f)
         #(Prefix (pandn Pq Qq) #f #(VEX (pandn Vdq Wdq) (vpandn Vdq Bdq Wdq) #f) #f)
         ;; 0F E0
         #(Prefix (pavgb Pq Qq) #f #(VEX (pavgb Vdq Wdq) (vpavgb Vdq Bdq Wdq) #f) #f)
         #(Prefix (psraw Pq Qq) #f #(VEX (psraw Vdq Wdq) (vpsraw Vdq Bdq Wdq) #f) #f)
         #(Prefix (psrad Pq Qq) #f #(VEX (psrad Vdq Wdq) (vpsrad Vdq Bdq Wdq) #f) #f)
         #(Prefix (pavgw Pq Qq) #f #(VEX (pavgw Wdq Wdq) (vpavgw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmulhuw Pq Qq) #f #(VEX (pmulhuw Vdq Wdq) (vpmulhuw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmulhw Pq Qq) #f #(VEX (pmulhw Vdq Wdq) (vpmulhw Vdq Bdq Wdq) #f) #f)
         #(Prefix #f
                  #(VEX (cvtdq2pd Vpd Wq) (vcvtdq2pd Vpd Wq/128))
                  #(VEX (cvttpd2dq Vq Wpd) (vcvttpd2dq Vq/128 Wpd))
                  #(VEX (cvtpd2dq Vq Wpd) (vcvtpd2dq Vq/128 Wpd)))
         #(Prefix (movntq Mq Pq) #f
                  #(VEX (movntdq Mdq Vdq) (vmovntdq Mdq Vdq) #f) #f)
         ;; 0F E8
         #(Prefix (psubsb Pq Qq) #f #(VEX (psubsb Vdq Wdq) (vpsubsb Vdq Bdq Wdq) #f) #f)
         #(Prefix (psubsw Pq Qq) #f #(VEX (psubsw Vdq Wdq) (vpsubsw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pminsw Pq Qq) #f #(VEX (pminsw Vdq Wdq) (vpminsw Vdq Bdq Wdq) #f) #f)
         #(Prefix (por Pq Qq) #f #(VEX (por Vdq Wdq) (vpor Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddsb Pq Qq) #f #(VEX (paddsb Vdq Wdq) (vpaddsb Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddsw Pq Qq) #f #(VEX (paddsw Vdq Wdq) (vpaddsw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmaxsw Pq Qq) #f #(VEX (pmaxsw Vdq Wdq) (vpmaxsw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pxor Pq Qq) #f #(VEX (pxor Vdq Wdq) (vpxor Vdq Bdq Wdq) #f) #f)
         ;; 0F F0
         #(Prefix #f #f #f #(VEX (lddqu Vpd Mdq) (vlddqu Vpd Mdq)))
         #(Prefix (psllw Pq Qq) #f #(VEX (psllw Vdq Wdq) (vpsllw Vdq Bdq Wdq) #f) #f)
         #(Prefix (pslld Pq Qq) #f #(VEX (pslld Vdq Wdq) (vpslld Vdq Bdq Wdq) #f) #f)
         #(Prefix (psllq Pq Qq) #f #(VEX (psllq Vdq Wdq) (vpsllq Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmuludq Pq Qq) #f #(VEX (pmuludq Vdq Wdq) (vpmuludq Vdq Bdq Wdq) #f) #f)
         #(Prefix (pmaddwd Pq Qq) #f #(VEX (pmaddwd Vdq Wdq) (vpmaddwd Vdq Bdq Wdq) #f) #f)
         #(Prefix (psadbw Pq Qq) #f #(VEX (psadbw Vdq Wdq) (vpsadbw Vdq Bdq Wdq) #f) #f)
         #(Prefix (maskmovq Pq Nq) #f
                  #(VEX (maskmovdqu Vdq Udq)
                        (vmaskmovdqu Vdq Udq)
                        #f)
                  #f)
         ;; 0F F8
         #(Prefix (psubb Pq Qq) #f #(VEX (psubb Vdq Wdq) (vpsubb Vdq Bdq Wdq) #f) #f)
         #(Prefix (psubw Pq Qq) #f #(VEX (psubw Vdq Wdq) (vpsubw Vdq Bdq Wdq) #f) #f)
         #(Prefix (psubd Pq Qq) #f #(VEX (psubd Vdq Wdq) (vpsubd Vdq Bdq Wdq) #f) #f)
         #(Prefix (psubq Pq Qq) #f #(VEX (psubq Vdq Wdq) (vpsubq Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddb Pq Qq) #f #(VEX (paddb Vdq Wdq) (vpaddb Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddw Pq Qq) #f #(VEX (paddw Vdq Wdq) (vpaddw Vdq Bdq Wdq) #f) #f)
         #(Prefix (paddd Pq Qq) #f #(VEX (paddd Vdq Wdq) (vpaddd Vdq Bdq Wdq) #f) #f)
         #f)
       ;; end of two-byte opcode table

       ;; 10
       (adc Eb Gb)
       (adc Ev Gv)
       (adc Gb Eb)
       (adc Gv Ev)
       (adc *AL Ib)
       (adc *rAX Iz)
       #(Mode (push *SS) #f)
       #(Mode (pop *SS) #f)
       ;; 18
       (sbb Eb Gb)
       (sbb Ev Gv)
       (sbb Gb Eb)
       (sbb Gv Ev)
       (sbb *AL Ib)
       (sbb *rAX Iz)
       #(Mode (push *DS) #f)
       #(Mode (pop *DS) #f)
       ;; 20
       (and Eb Gb)
       (and Ev Gv)
       (and Gb Eb)
       (and Gv Ev)
       (and *AL Ib)
       (and *rAX Iz)
       (*prefix* es)
       #(Mode (daa) #f)
       ;; 28
       (sub Eb Gb)
       (sub Ev Gv)
       (sub Gb Eb)
       (sub Gv Ev)
       (sub *AL Ib)
       (sub *rAX Iz)
       (*prefix* cs)
       #(Mode (das) #f)
       ;; 30
       (xor Eb Gb)
       (xor Ev Gv)
       (xor Gb Eb)
       (xor Gv Ev)
       (xor *AL Ib)
       (xor *rAX Iz)
       (*prefix* ss)
       #(Mode (aaa) #f)
       ;; 38
       (cmp Eb Gb)
       (cmp Ev Gv)
       (cmp Gb Eb)
       (cmp Gv Ev)
       (cmp *AL Ib)
       (cmp *rAX Iz)
       (*prefix* ds)
       #(Mode (aas) #f)
       ;; 40
       #(Mode (inc *eAX) (*prefix* rex))
       #(Mode (inc *eCX) (*prefix* rex rex.b))
       #(Mode (inc *eDX) (*prefix* rex rex.x))
       #(Mode (inc *eBX) (*prefix* rex rex.x rex.b))
       #(Mode (inc *eSP) (*prefix* rex rex.r))
       #(Mode (inc *eBP) (*prefix* rex rex.r rex.b))
       #(Mode (inc *eSI) (*prefix* rex rex.r rex.x))
       #(Mode (inc *eDI) (*prefix* rex rex.r rex.x rex.b))
       ;; 48
       #(Mode (dec *eAX) (*prefix* rex rex.w))
       #(Mode (dec *eCX) (*prefix* rex rex.w rex.b))
       #(Mode (dec *eDX) (*prefix* rex rex.w rex.x))
       #(Mode (dec *eBX) (*prefix* rex rex.w rex.x rex.b))
       #(Mode (dec *eSP) (*prefix* rex rex.w rex.r))
       #(Mode (dec *eBP) (*prefix* rex rex.w rex.r rex.b))
       #(Mode (dec *eSI) (*prefix* rex rex.w rex.r rex.x))
       #(Mode (dec *eDI) (*prefix* rex rex.w rex.r rex.x rex.b))
       ;; 50
       #(d64 (push *rAX/r8))
       #(d64 (push *rCX/r9))
       #(d64 (push *rDX/r10))
       #(d64 (push *rBX/r11))
       #(d64 (push *rSP/r12))
       #(d64 (push *rBP/r13))
       #(d64 (push *rSI/r14))
       #(d64 (push *rDI/r15))
       ;; 58
       #(d64 (pop *rAX/r8))
       #(d64 (pop *rCX/r9))
       #(d64 (pop *rDX/r10))
       #(d64 (pop *rBX/r11))
       #(d64 (pop *rSP/r12))
       #(d64 (pop *rBP/r13))
       #(d64 (pop *rSI/r14))
       #(d64 (pop *rDI/r15))
       ;; 60
       #(Mode #(Datasize (pushaw)
                         (pushad)
                         #f)
              #f)
       #(Mode #(Datasize (popaw)
                         (popad)
                         #f)
              #f)
       #(Mode (bound Gv Ma)
              #f)
       #(Mode (arpl Ew Gw)
              (movsxd Gv Ed))
       (*prefix* fs)
       (*prefix* gs)
       (*prefix* operand)
       (*prefix* address)
       ;; 68
       #(d64 (push Iz))
       (imul Gv Ev Iz)
       #(d64 (push IbS))
       (imul Gv Ev IbS)
       (ins Yb *DX)
       (ins Yz *DX)
       (outs *DX Xb)
       (outs *DX Xz)
       ;; 70
       #(f64 (jo Jb))
       #(f64 (jno Jb))
       #(f64 (jb Jb))
       #(f64 (jnb Jb))
       #(f64 (jz Jb))
       #(f64 (jnz Jb))
       #(f64 (jbe Jb))
       #(f64 (jnbe Jb))
       ;; 78
       #(f64 (js Jb))
       #(f64 (jns Jb))
       #(f64 (jp Jb))
       #(f64 (jnp Jb))
       #(f64 (jl Jb))
       #(f64 (jnl Jb))
       #(f64 (jle Jb))
       #(f64 (jnle Jb))
       ;; 80
       #(Group "Group 1"
               #((add Eb Ib) (or Eb Ib) (adc Eb Ib) (sbb Eb Ib)
                 (and Eb Ib) (sub Eb Ib) (xor Eb Ib) (cmp Eb Ib)))
       #(Group "Group 1"
               #((add Ev Iz) (or Ev Iz) (adc Ev Iz) (sbb Ev Iz)
                 (and Ev Iz) (sub Ev Iz) (xor Ev Iz) (cmp Ev Iz)))
       #(Mode #(Group "Redundant Group 1"
                      #((add Eb Ib) (or Eb Ib) (adc Eb Ib) (sbb Eb Ib)
                        (and Eb Ib) (sub Eb Ib) (xor Eb Ib) (cmp Eb Ib)))
              #f)
       #(Group "Group 1"
               #((add Ev IbS) (or Ev IbS) (adc Ev IbS) (sbb Ev IbS)
                 (and Ev IbS) (sub Ev IbS) (xor Ev IbS) (cmp Ev IbS)))
       (test Eb Gb)
       (test Ev Gv)
       (xchg Eb Gb)
       (xchg Ev Gv)
       ;; 88
       (mov Eb Gb)
       (mov Ev Gv)
       (mov Gb Eb)
       (mov Gv Ev)
       (mov Ew Sw)
       (lea Gv M)
       (mov Sw Ew)
       #(Group "Group 1A"               ;Three byte XOP prefix
               #(#(d64 (pop Ev))
                 #f #f #f #f #f #f #f))
       ;; 90
       (*nop*)
       (xchg *rCX/r9 *rAX)
       (xchg *rDX/r10 *rAX)
       (xchg *rBX/r11 *rAX)
       (xchg *rSP/r12 *rAX)
       (xchg *rBP/r13 *rAX)
       (xchg *rSI/r14 *rAX)
       (xchg *rDI/r15 *rAX)
       ;; 98
       #(Datasize (cbw) (cwde) (cdqe))
       #(Datasize (cwd) (cdq) (cqo))
       #(Mode (callf Ap) #f)
       (fwait)
       #(Mode #(Datasize (pushfw)
                         (pushfd)
                         #f)
              #(Datasize (pushfw)
                         (pushfq)
                         (pushfq)))
       #(Mode #(Datasize (popfw)
                         (popfd)
                         #f)
              #(Datasize (popfw)
                         (popfq)
                         (popfq)))
       (sahf)
       (lahf)
       ;; A0
       (mov *AL Ob)
       (mov *rAX Ov)
       (mov Ob *AL)
       (mov Ov *rAX)
       (movs Yb Xb)
       (movs Yv Xv)
       (cmps Xb Yb)
       (cmps Xv Yv)
       ;; A8
       (test *AL Ib)
       (test *rAX Iz)
       (stos Yb *AL)
       (stos Yv *rAX)
       (lods *AL Xb)
       (lods *rAX Xv)
       (scas *AL Yb)
       (scas *rAX Yv)
       ;; B0
       (mov *AL/R8L Ib)
       (mov *CL/R9L Ib)
       (mov *DL/R10L Ib)
       (mov *BL/R11L Ib)
       (mov *AH/R12L Ib)
       (mov *CH/R13L Ib)
       (mov *DH/R14L Ib)
       (mov *BH/R15L Ib)
       ;; B8
       (mov *rAX/r8 Iv)
       (mov *rCX/r9 Iv)
       (mov *rDX/r10 Iv)
       (mov *rBX/r11 Iv)
       (mov *rSP/r12 Iv)
       (mov *rBP/r13 Iv)
       (mov *rSI/r14 Iv)
       (mov *rDI/r15 Iv)
       ;; C0
       #(Group "Shift Group 2"
               #((rol Eb Ib) (ror Eb Ib) (rcl Eb Ib)
                 (rcr Eb Ib) (shl Eb Ib) (shr Eb Ib) #f (sar Eb Ib)))
       #(Group "Shift Group 2"
               #((rol Ev Ib) (ror Ev Ib) (rcl Ev Ib)
                 (rcr Ev Ib) (shl Ev Ib) (shr Ev Ib) #f (sar Ev Ib)))
       #(f64 (ret Iw))
       #(f64 (ret))
       #(Mode (les Gz Mp) #f)           ;Three byte VEX prefix
       #(Mode (lds Gz Mp) #f)           ;Two byte VEX prefix
       #(Group "Group 11"
               #((mov Eb Ib) #f #f #f #f #f #f #f))
       #(Group "Group 11"
               #((mov Ev Iz) #f #f #f #f #f #f #f))
       ;; C8
       (enter Iw Ib)
       #(d64 (leave))
       (retf Iw)
       (retf)
       (int3)
       (int Ib)
       #(Mode (into) #f)
       #(Datasize (iretw)
                  (iretd)
                  (iretq))
       ;; D0
       #(Group "Shift Group 2"
               #((rol Eb *unity) (ror Eb *unity) (rcl Eb *unity) (rcr Eb *unity)
                 (shl Eb *unity) (shr Eb *unity) #f (sar Eb *unity)))
       #(Group "Shift Group 2"
               #((rol Ev *unity) (ror Ev *unity) (rcl Ev *unity) (rcr Ev *unity)
                 (shl Ev *unity) (shr Ev *unity) #f (sar Ev *unity)))
       #(Group "Shift Group 2"
               #((rol Eb *CL) (ror Eb *CL) (rcl Eb *CL)
                 (rcr Eb *CL) (shl Eb *CL) (shr Eb *CL) #f (sar Eb *CL)))
       #(Group "Shift Group 2"
               #((rol Ev *CL) (ror Ev *CL) (rcl Ev *CL)
                 (rcr Ev *CL) (shl Ev *CL) (shr Ev *CL) #f (sar Ev *CL)))
       #(Mode (aam Ib) #f)
       #(Mode (aad Ib) #f)
       #(Mode (salc) #f)
       (xlatb)
       ;; D8: x87 escape
       #(Group "x87 D8"
               #((fadd Md)
                 (fmul Md)
                 (fcom Md)
                 (fcomp Md)
                 (fsub Md)
                 (fsubr Md)
                 (fdiv Md)
                 (fdivr Md))
               #((fadd *st0 *st)
                 (fmul *st0 *st)
                 (fcom *st0 *st)
                 (fcomp *st0 *st)
                 (fsub *st0 *st)
                 (fsubr *st0 *st)
                 (fdiv *st0 *st)
                 (fdivr *st0 *st)))
       #(Group "x87 D9"
               #((fld Md)
                 #f
                 (fst Md)
                 (fstp Md)
                 (fldenv M)
                 (fldcw Mw)
                 (fnstenv M)
                 (fnstcw Mw))
               #((fld *st0 *st)
                 (fxch *st0 *st)
                 #((fnop) #f #f #f #f #f #f #f)
                 #f
                 #((fchs) (fabs) #f #f (ftst) (fxam) #f #f)
                 #((fld1) (fldl2t) (fldl2e) (fldpi) (fldlg2) (fldln2) (fldz) #f)
                 #((f2xm1) (fyl2x) (fptan) (fpatan) (fxtract) (fprem1) (fdecstp) (fincstp))
                 #((fprem) (fyl2xp1) (fsqrt) (fsincos) (frndint) (fscale) (fsin) (fcos))))
       #(Group "x87 DA"
               #((fiadd Md)
                 (fimul Md)
                 (ficom Md)
                 (ficomp Md)
                 (fisub Md)
                 (fisubr Md)
                 (fidiv Md)
                 (fidivr Md))
               #((fcmovb *st0 *st)
                 (fcmove *st0 *st)
                 (fcmovbe *st0 *st)
                 (fcmovu *st0 *st)
                 #f
                 #(#f (fucompp) #f #f #f #f #f #f)
                 #f
                 #f))
       #(Group "x87 DB"
               #((fild Md)
                 (fisttp Md)
                 (fist Md)
                 (fistp Md)
                 #f
                 (fld Mem80)
                 #f
                 (fstp Mem80))
               #((fcmovnb *st0 *st)
                 (fcmovne *st0 *st)
                 (fcmovnbe *st0 *st)
                 (fcmovnu *st0 *st)
                 #(#f #f (fnclex) (fninit) #f #f #f #f)
                 (fucomi *st0 *st)
                 (fcomi *st0 *st)
                 #f))
       #(Group "x87 DC"
               #((fadd Mq)
                 (fmul Mq)
                 (fcom Mq)
                 (fcomp Mq)
                 (fsub Mq)
                 (fsubr Mq)
                 (fdiv Mq)
                 (fdivr Mq))
               #((fadd *st *st0)
                 (fmul *st *st0)
                 #f
                 #f
                 (fsub *st *st0)
                 (fsubr *st *st0)
                 (fdivr *st *st0)
                 (fdiv *st *st0)))
       #(Group "x87 DD"
               #((fld Mq)
                 (fisttp Mq)
                 (fst Mq)
                 (fstp Mq)
                 (frstor M)
                 #f
                 (fnsave M)
                 (fnstsw Mw))
               #((ffree *st)
                 #f
                 (fst *st)
                 (fstp *st)
                 (fucom *st *st0)
                 (fucomp *st)
                 #f
                 #f))
       #(Group "x87 DE"
               #((fiadd Mw)
                 (fimul Mw)
                 (ficom Mw)
                 (ficomp Mw)
                 (fisub Mw)
                 (fisubr Mw)
                 (fidiv Mw)
                 (fdivr Mw))
               #((faddp *st *st0)
                 (fmulp *st *st0)
                 #f
                 #(#f (fcompp) #f #f #f #f #f #f)
                 (fsubrp *st *st0)
                 (fsubp *st *st0)
                 (fdivrp *st *st0)
                 (fdivp *st *st0)))
       #(Group "x87 DF"
               #((fild Mw)
                 (fisttp Mw)
                 (fist Mw)
                 (fistp Mw)
                 (fbld Mem80)
                 (fild Mq)
                 (fbstp Mem80)
                 (fistp Mq))
               #(#f
                 #f
                 #f
                 #f
                 #((fnstsw *AX) #f #f #f #f #f #f #f)
                 (fucomip *st0 *st)
                 (fcomip *st0 *st)
                 #f))
       ;; E0
       #(f64 (loopnz Jb))
       #(f64 (loopz Jb))
       #(f64 (loop Jb))
       #(Mode #(Addrsize (jcxz Jb)
                         (jecxz Jb)
                         #f)
              #(Addrsize #f
                         #(f64 (jecxz Jb))
                         #(f64 (jrcxz Jb))))
       (in *AL Ib)
       (in *eAX Ib)
       (out Ib *AL)
       (out Ib *eAX)
       ;; E8
       #(f64 (call Jz))
       #(f64 (jmp Jz))
       #(Mode (jmpf Ap) #f)
       #(f64 (jmp Jb))
       (in *AL *DX)
       (in *eAX *DX)
       (out *DX *AL)
       (out *DX *eAX)
       ;; F0
       (*prefix* lock)
       (icebp)
       (*prefix* repnz)
       (*prefix* repz)
       (hlt)
       (cmc)
       #(Group "Unary Group 3"
               #((test Eb Ib) (test/amd Eb Ib) (not Eb) (neg Eb)
                 (mul Eb) (imul Eb) (div Eb) (idiv Eb)))
       #(Group "Unary Group 3"
               #((test Ev Iz) (test/amd Ev Iz) (not Ev) (neg Ev)
                 (mul Ev) (imul Ev) (div Ev) (idiv Ev)))
       ;; F8
       (clc) (stc)
       (cli) (sti)
       (cld) (std)
       #(Group "Group 4"
               #((inc Eb) (dec Eb) #f #f #f #f #f #f))
       #(Group "Group 5"
               #((inc Ev)
                 (dec Ev)
                 #(f64 (call Ev))
                 (callf Mp)
                 #(f64 (jmp Ev))
                 (jmpf Mp)
                 #(d64 (push Ev))
                 #f))))

  ;; AMDs XOP opcode maps are not part of the other opcode map.
  (define XOP-opcode-map-8
    '#(#f #f #f #f #f #f #f #f
       ;; 08
       #f #f #f #f #f #f #f #f
       ;; 10
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 20
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 30
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 40
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 50
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 60
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 70
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 80
       #f #f #f #f #f
       #(VEX #f (vpmacssww Vo Ho Wo Lo))
       #(VEX #f (vpmacsswd Vo Ho Wo Lo))
       #(VEX #f (vpmacssdql Vo Ho Wo Lo))
       #f #f #f #f #f #f
       #(VEX #f (vpmacssdd Vo Ho Wo Lo))
       #(VEX #f (vpmacssdqh Vo Ho Wo Lo))
       ;; 90
       #f #f #f #f #f
       #(VEX #f (vpmacsww Vo Ho Wo Lo))
       #(VEX #f (vpmacswd Vo Ho Wo Lo))
       #(VEX #f (vpmacsdql Vo Ho Wo Lo))
       #f #f #f #f #f #f
       #(VEX #f (vpmacsdd Vo Ho Wo Lo))
       #(VEX #f (vpmacsdqh Vo Ho Wo Lo))
       ;; A0
       #f #f
       #(VEX #f #(W (vpcmov Vx Hx Wx Lx) (vpcmov Vx Hx Lx Wx)))
       #(VEX #f #(W (vpperm Vo Ho Wo Lo) (vpperm Vo Ho Lo Wo)))
       #f #f
       #(VEX #f (vpmadcsswd Vo Ho Wo Lo))
       #f
       #f #f #f #f #f #f #f #f
       ;; B0
       #f #f #f #f #f #f
       #(VEX #f (vpmadcswd Vo Ho Wo Lo))
       #f
       #f #f #f #f #f #f #f #f
       ;; C0
       #(VEX #f (vprotb Vo Wo Ib))
       #(VEX #f (vprotw Vo Wo Ib))
       #(VEX #f (vprotd Vo Wo Ib))
       #(VEX #f (vprotq Vo Wo Ib))
       #f #f #f #f #f #f #f #f
       #f #f #f #f                      ;TODO
       ;; D0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; E0
       #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f                      ;TODO
       ;; F0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

  (define XOP-opcode-map-9
    '#(#f
       #(VEX #f
             #(Group "XOP group #1"
                    #(#f (blcfill By Ed/q) (blsfill By Ed/q) (blcs By Ed/q)
                         (tzmsk By Ed/q) (blcic By Ed/q) (blsic By Ed/q)
                         (t1mskc By Ed/q))))
       #(VEX #f
             #(Group "XOP group #2"
                    #(#f (blcmsk By Ed/q) #f #f
                         #f #f (blci By Ed/q) #f)))
       #f #f #f #f #f
       ;; 08
       #f #f #f #f #f #f #f #f
       ;; 10
       #f #f
       #(VEX #f
             #(Group "XOP group #3"
                     #((llwpcb Rd/q) (slwpcb Rd/q) #f #f #f #f #f #f)))
       #f #f #f #f #f
       ;; 18
       #f #f #f #f #f #f #f #f
       ;; 20
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 30
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 40
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 50
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 60
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 70
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 80
       #(VEX #f (vfrczps Vx Wx))
       #(VEX #f (vfrczpd Vx Wx))
       #(VEX #f (vfrczss Vq Wss))
       #(VEX #f (vfrczsd Vq Wsd))
       #f #f #f #f
       #f #f #f #f                ;AMD's opcode map places VPSHA* here
       #f #f #f #f
       ;; 90
       #(VEX #f #(W (vprotb Vo Wo Ho) (vprotb Vo Ho Wo)))
       #(VEX #f #(W (vprotw Vo Wo Ho) (vprotw Vo Ho Wo)))
       #(VEX #f #(W (vprotd Vo Wo Ho) (vprotd Vo Ho Wo)))
       #(VEX #f #(W (vprotq Vo Wo Ho) (vprotq Vo Ho Wo)))
       #(VEX #f #(W (vpshlb Vo Wo Ho) (vpshlb Vo Ho Wo)))
       #(VEX #f #(W (vpshlw Vo Wo Ho) (vpshlw Vo Ho Wo)))
       #(VEX #f #(W (vpshld Vo Wo Ho) (vpshld Vo Ho Wo)))
       #(VEX #f #(W (vpshlq Vo Wo Ho) (vpshlq Vo Ho Wo)))
       #(VEX #f #(W (vpshab Vo Wo Ho) (vpshab Vo Ho Wo)))
       #(VEX #f #(W (vpshaw Vo Wo Ho) (vpshaw Vo Ho Wo)))
       #(VEX #f #(W (vpshad Vo Wo Ho) (vpshad Vo Ho Wo)))
       #(VEX #f #(W (vpshaq Vo Wo Ho) (vpshaq Vo Ho Wo)))
       #f #f #f #f
       ;; A0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; B0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; C0
       #f
       #(VEX #f (vphaddbw Vo Wo))
       #(VEX #f (vphaddbd Vo Wo))
       #(VEX #f (vphaddbq Vo Wo))
       #f #f
       #(VEX #f (vphaddwd Vo Wo))
       #(VEX #f (vphaddwq Vo Wo))
       #f #f #f
       #(VEX #f (vphadddq Vo Wo))
       #f #f #f #f
       ;; D0
       #f
       #(VEX #f (vphaddubw Vo Wo))      ;"VPHADDUBWD"?
       #(VEX #f (vphaddubd Vo Wo))
       #(VEX #f (vphaddubq Vo Wo))
       #f #f
       #(VEX #f (vphadduwd Vo Wo))
       #(VEX #f (vphadduwq Vo Wo))
       #f #f #f
       #(VEX #f (vphaddudq Vo Wo))
       #f #f #f #f
       ;; E0
       #f
       #(VEX #f (vphsubbw Vo Wo))
       #(VEX #f (vphsubwd Vo Wo))
       #(VEX #f (vphsubdq Vo Wo))
       #f #f #f #f #f #f #f #f #f #f #f #f
       ;; F0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

  (define XOP-opcode-map-A
    '#(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 10
       #(Prefix (bextr Gd/q Ed/q Id) #f #f #f #f)
       #f
       #(VEX #f
             #(Group "XOP group #4"
                     #((lwpins By Ed Id) (lwpval By Ed Id) #f #f #f #f #f #f)))
       #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 20
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 30
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 40
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 50
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 60
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 70
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 80
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; 90
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; A0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; B0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; C0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; D0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; E0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       ;; F0
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
