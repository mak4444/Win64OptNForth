\ hdasmx64.fth - x64 disassembler for VFX Forths

REQUIRE [IF] ~mak/CompIF3.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f

(*
Original code by CCS, adapted for ProForth by MPE, released
by CCS to MPE for use with ProForth, VFX Forth and cross
compilers.


tel: +44 (0)23 8063 1441
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com

From North America, our telephone numbers are:
  011 44 23 8063 1441

Usage:

DISASM/al       \ addr len --
  Disassemble from addr for len bytes. NEXT is displayed as individual
  instructions.

disasm/ft       \ from to --
  Disassemble from address FROM to address TO. The NEXT macro is displayed
  as a NEXT, macro.

disasm/f        \ addr --
  Disassemble from address FROM. The NEXT, macro is displayed as a NEXT,
  macro. The display stops when the first NEXT, macro is encountered with
  no outstanding forward branch, or if the forward branch is over 256 bytes.

dasm            \ -- ; disassemble word; DASM <word>
  Disassemble from the cfa of <word>. The NEXT, macro is displayed as a NEXT,
  macro. The display stops when the first NEXT, macro is encountered with
  no outstanding forward branch, or if the forward branch is over 256 bytes.

All the utility words can be found in the DISASSEMBLER vocabulary.

20230101 SFP012 Added EMMS
20200502 SFP011 Added MOVSXD
20200423 SFP010 More 64 bit updates, fixed CMPXCHG for bytes.
20190610 SFP009 Increased output text buffer size from 80 to 128 bytes.
20181009 SFP008 More 64 bit updates.
20181003 SFP007	RIP mode display
20170815 SFP006 Added REX decoding.
20170517 SFP005 Started 64 bit conversion.
SFP004	10 Sept 1999	Added display of disassembled code size.
			Added display of token name.
			Added extra double-bracket to avoid
			colouring problem in WinEdit.
SFP003  11 July 1999    Added display of branch target address
SFP002  30 July 1998    Modified XDASM to look for labels as well as words
SFP001  29 June 1998    Converted for cross compiler
*)

REQUIRE NUMBER? ~mak/lib/fpcnum.f

REQUIRE CASE lib/ext/case.f
REQUIRE { ~mak\locals.f
REQUIRE NEAR_NFA ~mak\NEARNFA.4TH
REQUIRE forth ~mak/lib/caseins.f
\+ CASE-INS CASE-INS ON

\ ==========
\ *! dasmx64
\ *T Disassembler
\ ==========
\ *P VFX Forth includes a disassembler for debugging purposes.
\ ** Native code built by the system can be viewed at the machine
\ ** code level.



\ *****************
\ VFX harness words
\ *****************

: flip       \ n -- n' ; swap low word of n
  dup $FF and 8 lshift
  over $FF00 and 8 rshift      \ n hi lo --
  or swap $FFFF0000 and or
;

: split      \ n -- lo-word high-word
  dup $FFFF and swap 16 rshift $FFFF and
;
[IFNDEF] MAKELONG
: MAKELONG      \ lo hi -- 32bit
\ *G Given two 16 bit numbers produce a single 32 bit one.
  16 lshift  swap $FFFF and  or
;
[THEN]

: scale         \ n cnt -- n' ; -CNT shift aright, +CNT shift aleft
  dup 0<
  if  abs rshift  else  lshift  then
;

: <C		\ char -- n ; sign extend byte
  dup $080 and negate or
\  DUP $7F >
\  IF  $FFFFFF00 OR  THEN
;

: <w            \ w -- n ; sign extend word
  dup $8000 and negate or
\  dup $07FFF >
\  if  $FFFF0000 or  endif
;

: <l		\ l -- n ; sign extend 32 bit
  dup 1 noop 31 lshift and negate or
;

[undefined] either= [if]
: either=       \ n a b -- t/f ; true if n=a or n=b
  2 pick = -rot                 \ n, b
  =  or                         \ n, a
;
[then]

Create NextVal    \ -- addr ; needed to find NEXT
  $0C3 c,

1 constant NextWidth	\ -- u ; width of return instruction


\ ***********************
\ WORDS NOT IN CCS KERNEL
\ ***********************

: digits        \ d n -- d'
  0 ?do  #  loop  ;

: DSTR0         ( d width -- adr cnt )
  >R <# R> digits #>  ;

: STR0          ( n width -- adr cnt )
  >r  s>d <#  r> digits  #>
;

: CS0
  1 ;

: =cells        \ addr -- addr' ; adjust to boundary - PFW does not need this
  aligned
;

: BYTE-JUMPER	\ addr n -- addr'
  0 2DUP + IF
    DO  COUNT + ( aligned )  LOOP
  ELSE
    2DROP
  THEN ;

: u<=           \ u1 u2 -- flag
  u> not
;

: u>=           \ u1 u2 -- flag
  u< not
;


\ *************************
\ DISASSEMBLER main portion
\ *************************
\ --- VARS

VARIABLE (DisAddr)              \ current disassembly address
VARIABLE ToDisAddr              \ to address in from to words
VARIABLE (DisSeg)               \ current disassembly segment
variable branch-target          \ highest branch target address
variable next-found             \ true when NEXT found

\- '(S") ' (S") VALUE '(S")

MODULE: disassembler

Defer NDPins?	\ -- flag ; NDP instruction and return true, otherwise false
  ' false to NDPins?

: DisAddr  ( -- addr )
   \ current disassembly address
   (DisAddr)
 @
 ;

: SET-DisAddr ( addr -- )
   \ set current disassembly addr to addr
   (DisAddr) ! ;

: ADD-DisAddr ( n -- )
   \ increment current disassembly address with n
   (DisAddr) +! ;

: DEC-DisAddr ( -- )
   \ increment current disassembly address with n
   DisAddr 1- SET-DisAddr ;

: SET-DisSeg ( n -- )
   (DisSeg) ! ;

: DisSeg ( -- n )
   (DisSeg) @ ;

: >branch-target        \ addr -- ; insert if higher than current value
  dup branch-target @ u>
  if  branch-target !  else  drop  THEN
;


\ --- FLAGS FOR INSTRUCTION CONDITIONS
\ VARIABLE (32bit?)	\ flag for current disassembly width
VARIABLE (wbit)		\ true if opcode has W bit in bit0 ))
VARIABLE ($66?)		\ data size override
VARIABLE ($67?)		\ address size override
\ VARIABLE (16/32?)	\ only 16 bit & 32 bit registers
VARIABLE (8/16?)	\ only  8 bit & 16 bit registers allowed
VARIABLE (16bit?)	\ only 16 bit regs and mem allowed
\ VARIABLE (8bit?)	\ only  8 bit registers and memory allowed ; SFP006

variable DefAsize	\ default address size; 64/32/16
  64 DefAsize !
VARIABLE opcAsize	\ opcode address size; 64/32/16
variable DefDsize	\ default data size; 32/16/8 - 64 set by REX.W
  32 DefDsize !
VARIABLE opcDsize	\ opcode data size; 64/32/16/8

VARIABLE Opcode1	\ first byte of instruction
VARIABLE Opcode2	\ second byte of instruction
\ lower bits of opcode
VARIABLE (bit0)
VARIABLE (bit1)
VARIABLE (bit2)
VARIABLE (bit3)

variable rex-byte	\ 40..4F
variable prefix-byte	\ 66, F2 or F3
(*
  7                           0
+---+---+---+---+---+---+---+---+
| 0   1   0   0 | W | R | X | B |
+---+---+---+---+---+---+---+---+
W	When 1, a 64-bit operand size is used. Otherwise,
        when 0, the default operand size is used (which is 32-bit
        for most but not all instructions).
R	This 1-bit value is an extension to the MODRM.reg field.
X	This 1-bit value is an extension to the SIB.index field.
B	This 1-bit value is an extension to the MODRM.rm field
	or the SIB.base field.
*)
: rex?		\ -- flag ; true if REX byte
  rex-byte @ $040 and  ;
: rex.w?	\ -- flag ; true if REX.W
  rex-byte @ $048 and $048 = ;
: rex.r?	\ -- flag ; true if REX.R
  rex-byte @ $044 and $044 =  ;
: rex.x?	\ -- flag ; true if REX.X
  rex-byte @ $042 and $042 =  ;
: rex.b?	\ -- flag ; true if REX.B
  rex-byte @ $041 and $041 =  ;

\ --- SET VARIABLES AND FLAGS
\- >= : >= < 0= ;

: D64bit?	\ -- flag
  opcDsize @ 64 =  ;
: D32bit?	\ -- flag
  opcDsize @ 32 >=  ;

: Mode32bit?	\ -- flag
  DefAsize @ 32 >=  ;
: Mode16bit?	\ -- flag
  DefAsize @ 16 =  ;

: +wbit		\ -- ; set (w) field length operand in this instruction
  (wbit) ON ;			\ ))
: -wbit		\ -- ; zero (w) field length operand in this instruction
  (wbit) OFF ;			\ ))
: wbit?		\ -- flag ; status of (w) field flag
  (wbit) @ ;			\ ))

: SET-$66?	\ -- ; set data size overide flag
  ($66?) ON ;
: ZERO-$66?	\ -- ; zero data size overide flag
  ($66?) OFF ;
: $66?		\ -- flag ; status of overide flag
  ($66?) @ ;


\ More flag handling

: SET-8/16? ( -- )
   \ set 8/16 bit regs only flag
   (8/16?) ON ;

: ZERO-8/16? ( -- )
   \ zero 8/16 bit regs only flag
   (8/16?) OFF ;

: 8/16? ( -- flag )
   (8/16?) @ ;

: SET-16bit? ( -- )
   \ set 16 bit regs only flag
   (16bit?) ON ;

: ZERO-16bit? ( -- )
   \ zero 16 bit regs only flag
   (16bit?) OFF ;

: 16bit? ( -- flag )
   (16bit?) @ ;

: SET-bit0 ( n -- )
   \ store n in bit0 flag
   (bit0) ! ;

: ZERO-bit0 ( -- )
   (bit0) OFF ;

: bit0 ( -- flag )
   (bit0) @ ;

: ?8bit0	\ -- ; if bit 0 = 0, it's an 8 bit operation
  bit0 0=
  if  8 opcDsize !  then
;

: SET-bit1	\ n -- ; store n in bit1 flag
  (bit1) ! ;

: ZERO-bit1 ( -- )
   (bit1) OFF ;

: bit1 ( -- flag )
   (bit1) @ ;

: SET-bit2 ( n -- )
   \ store n in bit2 flag
   (bit2) ! ;

: ZERO-bit2 ( -- )
   (bit2) OFF ;

: bit2 ( -- flag )
   (bit2) @ ;

: SET-bit3 ( n -- )
   \ store n in bit3 flag
   (bit3) ! ;

: ZERO-bit3 ( -- )
   (bit3) OFF ;

: bit3 ( -- flag )
   (bit3) @ ;

: INIT-VARS  ( -- )
   Opcode1 OFF
   Opcode2 OFF
   +wbit		\ assume (w) operand length field
\   ZERO-8bit?		\ untrue 8 bit regs and mem ; del SFP006
   ZERO-8/16?		\ untrue 8 bit and 16 bit flag only
   ZERO-16bit?		\ untrue 16 bit regs and mem only
\   ZERO-16/32?  	\ untrue 16 bit and 32 bit flag only
;

: setD16/64	\ -- ; if not 16 bit data, force 64 bit data
  $66? 0= if  64 opcDsize !  then
;

\ mod/rm byte handling

\ values of modrm byte
variable modrm-byte	\ byte itself
VARIABLE (r/m)		\ register/memory
VARIABLE (reg#)		\ register
VARIABLE (modifier)	\ modifier

: r/m  ( -- n )
  (r/m) @  ;
: reg#  ( -- n )
  (reg#) @  ;
: modifier ( -- n )
  (modifier) @  ;

: +rex>reg	\ ra -- rb ; allow for REX.R bit in the reg field
  rex.r? if  8 +  then  ;	\ SFP006
: +rex>rm	\ ra -- rb ; allow for REX.B bit in the rm field
  rex.b? if  8 +  then  ;	\ SFP006

: SET-r/m  ( n -- )
   (r/m) ! ;
: SET-reg#  ( n -- )
  (reg#) ! ;
: SET-modifier ( n -- )
  (modifier) ! ;

\ values of sib byte
VARIABLE (basereg)	\ reg used as base register
VARIABLE (indexreg)	\ reg used as index register
VARIABLE (ss)		\ scale factor

: basereg	( -- n )
  (basereg)  @ ;
: indexreg	( -- n )
  (indexreg) @  ;
: ss		( -- n )
  (ss) @  ;

: SET-basereg ( n -- )
  (basereg)  ! ;
: SET-indexreg ( n -- )
  (indexreg) ! ;
: SET-ss ( n -- )
   (ss)    ! ;

: +rex>base	\ ra -- rb ; allow for REX.B bit in the SIB base field
  rex.b? if  8 +  then  ;	\ SFP006
: +rex>index	\ ra -- rb ; allow for REX.X bit in the SIB index field
  rex.x? if  8 +  then  ;	\ SFP006


\ --- DISASSEMBLY OUTPUT

14 CONSTANT DisDumpCol  \ starting column for dump
38 CONSTANT DisOpCol    \ starting column for opcode
46 CONSTANT DisParmCol  \ starting column for parameters

128 constant LineSize	\ -- len ; size of output line buffer
 
CREATE (DisLine$) LineSize ALLOT	\ -- addr ; line buffer for disassembly
VARIABLE DisPos		\ -- addr ; output position for opcode info
VARIABLE DisDumpPos	\ -- addr ; output position for dump info

: DisLine$ ( -- addr cnt )
   (DisLine$) LineSize ;

: >DISLINE  ( adr cnt -- )   \ Add opcode information to disassembly line buffer.
   DisLine$ DisPos @  /STRING
   ROT MIN DUP
   DisPos  +!  \ increment print pos in dis buffer
   CMOVE ;

: >DISDUMP  ( adr cnt -- )   \ Add dump information to disassembly line buffer.
   DisLine$ DROP DisOpCol 1-     \ dump must not overwrite op
   DisDumpPos @ /STRING
   ROT MIN DUP
   DisDumpPos +!
   CMOVE ;

: BYTE>DUMP  ( x8 -- )
   2 STR0 >DISDUMP ;

: WORD>DUMP  ( x16 -- )
   FLIP 4 STR0 >DISDUMP ;

: LONG>DUMP  ( x32 -- )
   SPLIT FLIP SWAP FLIP MAKELONG
   8 STR0 >DISDUMP ;

: xlong>dump	\ x64 --
  dup long>dump  32 rshift long>dump
;

: BYTE>DIS  ( byte -- )
   2 STR0 >DISLINE ;

: WORD>DIS  ( word -- )
   4 STR0 >DISLINE ;

: LONG>DIS  ( long -- )
   8 STR0 >DISLINE ;

: xlong>dis	\ xlong --
  16 str0 >disline  ;

variable char-holder

: CHAR>DIS  ( char -- )
   char-holder c!
   char-holder 1 >DISLINE  ;

: [>DIS         ( --- )
   [char] [ CHAR>DIS ;

: ]>DIS         ( --- )
   [char] ] CHAR>DIS ;

: :>DIS         ( --- )
   [char] : CHAR>DIS ;

: +>DIS         ( --- )
   [char] + CHAR>DIS ;

: ->DIS         ( --- )
   [char] - CHAR>DIS ;

: ,>DIS         ( --- )
   s" , " >disline  ;

: ">DIS         ( --- )
   [char] " CHAR>DIS  ;

: #>dis		\ --
  [char] # char>dis  ;

: bl>dis	\ --
  $020 char>dis  ;

: $>dis		\ $addr --
  count >disline  ;

: DISADDR>DIS  ( -- )
   DisSeg CS0 <>
   IF DisSeg 4 STR0 >DISLINE :>DIS
   THEN
   DisAddr 8 STR0 >DISLINE ;

: DISINIT  ( -- )
  \ Initialize disassembly output buffer to current
  \ seg:address and output positions for dump and opcode.
  CS0 SET-DisSeg                        \ initialise segment
  DisLine$ BLANK                        \ initialise o/p buffer
  DisPos OFF
  DisDumpPos off

  s" ( " >disline			\ leading comment )
  DISADDR>DIS                           \ address of instruction

  DisDumpCol DisDumpPos !
  DisOpCol DisPos !
;

: disclose      \ -- ; add finishing text
  s"  )" >disdump
;


\ --- BIT-TESTS

: @BYTE  ( -- byte )
   DisAddr C@
   DUP BYTE>DUMP
   1 ADD-DisAddr ;

: @WORD  ( -- word )
\    DisSeg DisAddr W@L
   DisAddr W@
   DUP WORD>DUMP
   2 ADD-DisAddr ;

: @LONG  ( -- long )
\    DisSeg DisAddr @L
   DisAddr L@
   DUP LONG>DUMP
   4 ADD-DisAddr ;

: @XLONG  ( -- long )
\    DisSeg DisAddr @L
   DisAddr @
   DUP xlong>dump
   8 ADD-DisAddr ;

: @OPCODE  ( -- byte )
   @BYTE
   DUP 1 AND 0<> SET-bit0
   DUP 2 AND 0<> SET-bit1
   DUP 4 AND 0<> SET-bit2
   DUP 8 AND 0<> SET-bit3  ;

\  | bit7 | bit6 | bit5 | bit4 | bit3 | bit 2 | bit 1 | bit 0 |
\  |------|------|------|------|------|-------|-------|-------|
\  | mod  | mod  |opcode|opcode|opcode| r/m   | r/m   | r/m   |
\  |------|------|------|------|------|-------|-------|-------|
\    7                           0
\  +---+---+---+---+---+---+---+---+
\  | scale |   index   |    base   |
\  +---+---+---+---+---+---+---+---+

: .modrm	\ x --
  cr ." mod=" dup 6 rshift 3 and .
     ."  reg=" dup 3 rshift 7 and .
     ."  r/m=" 7 and .
;

: .sib	\ x --
  cr ." ss=" dup 6 rshift 3 and .
     ."  index=" dup 3 rshift 7 and .
     ."  base=" 7 and .
;

: SPLIT-BITS  ( byte -- bits76 bits543 bits012 )
   \ $38 = 00111000
   DUP -6 SCALE
   OVER $38 AND -3 SCALE
   ROT 7 AND ;

: @MODRM  ( -- )
   @BYTE dup modrm-byte !  SPLIT-BITS
   SET-r/m SET-reg# SET-modifier ;

: @SIB  ( -- )
   @BYTE SPLIT-BITS
   SET-basereg  SET-indexreg  SET-ss ;

: set-16/32/64	\ --
  opcDsize @ 8 = if
    s"  ILLEGAL data size " >disline
  THEN
;

\ --- OPCODE LABELS

: tops=		\ caddr len -- flag
\ Return true if the next len opcodes match those at caddr. On
\ success, the opcodes are read.
  { | #len -- }
  dup -> #len
  DisAddr swap  0 ?do			\ -- caddr target ; i= index
    over i + c@  over i + c@ <>	\ not same
    if  2drop 0  unloop exit  then	\ return 0
  loop
  #len 0 ?do  @byte drop  loop		\ step over target opcodes
  2drop -1
;

\- ", : ", '"' parse S", ;

CREATE OpCodes
  \ Opcodes in order of apearance.
  ( 00) ", ADD"    ", OR"     ", ADC"    ", SBB"
  ( 04) ", AND"    ", SUB"    ", XOR"    ", CMP"
  ( 08) ", INC"    ", DEC"    ", PUSH"   ", POP"
  ( 0C) ", DAA"    ", DAS"    ", AAA"    ", AAS"
  ( 10) ", PUSHA"  ", POPA"   ", BOUND"  ", ARPL"
  ( 14) ", IMUL"   ", INS"    ", OUTS"   ", J"
  ( 18) ", TEST"   ", XCHG"   ", MOV"    ", LEA"
  ( 1C) ", NOP"    ", CBW"    ", CWD"    ", CALL"
  ( 20) ", FWAIT"  ", PUSHF"  ", POPF"   ", SAHF"
  ( 24) ", LAHF"   ", MOVS"   ", CMPS"   ", STOS"
  ( 28) ", LODS"   ", SCAS"   ", RET"    ", LES"
  ( 2C) ", LDS"    ", ENTER"  ", LEAVE"  ", INT"
  ( 30) ", INTO"   ", IRET"   ", AAM"    ", AAD"
  ( 34) ", XLAT"   ", LOOPNE" ", LOOPE"  ", LOOP"
  ( 38) ", JCXZ"   ", IN"     ", OUT"    ", JMP"
  ( 3C) ", LOCK"   ", REPNE"  ", REP"    ", ESC"
  ( 40) ", HLT"    ", CMC"    ", CLC"    ", STC"
  ( 44) ", CLI"    ", STI"    ", CLD"    ", STD"
  ( 48) ", LAR"    ", LSL"    ", CLTS"   ", SET"
  ( 4C) ", BT"     ", SHLD"   ", BTS"    ", SHRD"
  ( 50) ", LSS"    ", BTR"    ", LFS"    ", LGS"
  ( 54) ", MOVZX"  ", BTC"    ", BSF"    ", BSR"
  ( 58) ", MOVSX"  ", ROL"    ", ROR"    ", RCL"
  ( 5C) ", RCR"    ", SHL"    ", SHR"    ", SAR"
  ( 60) ", NOT"    ", NEG"    ", MUL"    ", DIV"
  ( 64) ", IDIV"   ", SLDT"   ", STR"    ", LLDT"
  ( 68) ", LTR"    ", VERR"   ", VERW"   ", SGDT"
  ( 6C) ", SIDT"   ", LGDT"   ", LIDT"   ", SMSW"
  ( 70) ", LMSW"   ", CDQ"    ", ???"    ", CWDE"
  ( 74) ", POPAD"

: OPCODE  ( n -- adr cnt )
  OpCodes SWAP BYTE-JUMPER COUNT ;

CREATE CondCodes
  \ Condition codes for J and SET.
  ( 00) ", O"        ", NO"       ", B/NAE "   ", NB/AE "
  ( 04) ", Z/E "     ", NZ/NE "   ", BE/NA "   ", NBE/A "
  ( 08) ", S"        ", NS"       ", P/PE "    ", NP/PO "
  ( 0C) ", L/NGE "   ", NL/GE "   ", LE/NG "   ", NLE/G "

: CONDCODE  ( n -- adr cnt )
  CondCodes SWAP BYTE-JUMPER COUNT ;

CREATE SegRegs	 \ -- addr
  ", ES"  ", CS"  ", SS"  ", DS"  ", FS"  ", GS"

CREATE Regs	\ -- addr ; for classic 16 and 32 bit operation
  \  00      01      02      03
  ", AL"  ", CL"  ", DL"  ", BL"
  \  04      05      06      07
  ", AH"  ", CH"  ", DH"  ", BH"
  \  08      09      10      11
  ", AX"  ", CX"  ", DX"  ", BX"
  \  12      13      14      15
  ", SP"  ", BP"  ", SI"  ", DI"

create regs64	\ -- addr ; 64 bit registers
  ", RAX"  ", RCX"  ", RDX"  ", RBX"  ", RSP"  ", RBP"  ", RSI"  ", RDI"
  ", R8"   ", R9"   ", R10"  ", R11"  ", R12"  ", R13"  ", R14"  ", R15"
create regs32	\ -- addr ; 32 bit registers
  ", EAX"  ", ECX"  ", EDX"  ", EBX"  ", ESP"  ", EBP"  ", ESI"  ", EDI"
  ", R8D"  ", R9D"  ", R10D" ", R11D" ", R12D" ", R13D" ", R14D" ", R15D"
create regs16	\ -- addr ; 16 bit registers
  ", AX"   ", CX"   ", DX"   ", BX"   ", SP"   ", BP"   ", SI"   ", DI"
  ", R8W"  ", R9W"  ", R10W" ", R11W" ", R12W" ", R13W" ", R14W" ", R15W"
create regs8	\ -- addr ; 8 bit registers
  ", AL"   ", CL"   ", DL"   ", BL"   ", SPL"  ", BPL"  ", SIL"  ", DIL"
  ", R8L"  ", R9L"  ", R10L" ", R11L" ", R12L" ", R13L" ", R14L" ", R15L"
CREATE R/M16-Table	\ -- addr ; 16 bit mode only
  ", BX+SI"  ", BX+DI"  ", BP+SI"  ", BP+DI"
  ", SI"     ", DI"     ", BP"     ", BX"

: table$	( n table -- caddr len )  SWAP BYTE-JUMPER COUNT ;
: SEGREG	( n -- adr cnt )  SegRegs table$  ;
: REG		( n -- adr cnt )  Regs table$ ;
: R/M16		( n -- adr cnt )  R/M16-Table table$ ;

\ SFP006...
\ These are for 64 bit mode
: reg64$	( u -- caddr len )  regs64 table$  ;
: reg32$	( u -- caddr len )  regs32 table$  ;
: reg16$	( u -- caddr len )  regs16 table$  ;
: reg8$		( u -- caddr len )  regs8 table$  ;

: greg>dis	\ reg# size -- ; size = 64/32/16/8
  case
    64 of  reg64$ >disline  endof
    32 of  reg32$ >disline  endof
    16 of  reg16$ >disline  endof
    8  of  reg8$ >disline  endof
      true abort" Unknown register size
  endcase
;
: dreg>dis	\ reg# -- ; depends on data size
  opcDsize @ greg>dis
;
: areg>dis	\ reg# -- ; depends on address size
  opcAsize @ greg>dis
;
\ ...SFP006

: PTR?		\ -- flag ; mod=0
\   modifier 0=  r/m 4 <> AND ;
  modifier 3 <  ;


: PTR>DIS	\ -- ; display memory size
  case  opcDsize @
    64 of  s" QWord "  endof
    32 of  s" DWord "  endof
    16 of  s" Word "  endof
    8  of  s" Byte "  endof
      true abort" unknown data size"
  endcase
  >disline
;
(*
: PTR>DIS	\ -- ; display memory size
  case  opcDsize @
    64 of  s" QWord Ptr "  endof
    32 of  s" DWord Ptr "  endof
    16 of  s" Word Ptr "  endof
    8  of  s" Byte Ptr "  endof
      true abort" unknown data size"
  endcase
  >disline
;
*)

\ --- PARAMETER DECODING
\
\     76543210        76543210
\          --- r/m         --- base
\       ---    ttt      ---    index
\     --       mod    --       ss
\
\  size = 16                                          mod = 3
\ r/m   mod = 0     mod = 1     mod = 2        w = 0    w = 1
\ ---   ----------  ----------  -------------  --------------
\ 0     [BX+SI]     [BX+SI+d8]  [BX+SI+d16]    AL       AX
\ 1     [BX+DI]     [BX+DI+d8]  [BX+DI+d16]    CL       CX
\ 2     [BP+SI]     [BP+SI+d8]  [BP+SI+d16]    DL       DX
\ 3     [BP+DI]     [BP+DI+d8]  [BP+DI+d16]    BL       BX
\ 4     [SI]        [SI+d8]     [SI+d16]       AH       SP
\ 5     [DI]        [DI+d8]     [DI+d16]       CH       BP
\ 6     d16         [BP+d8]     [BP+d16]       DH       SI
\ 7     [BX]        [BX+d8]     [BX+d16]       BH       DI
\
\ size = 32                                          mod = 3
\    r/m   mod = 0     mod = 1     mod = 2       w = 0    w = 1
\    ---   ----------  ----------  ------------- --------------
\    0     [EAX]       [EAX+d8]    [EAX+d32]     AL       EAX
\    1     [ECX]       [ECX+d8]    [ECX+d32]     CL       ECX
\    2     [EDX]       [EDX+d8]    [EDX+d32]     DL       EDX
\    3     [EBX]       [EBX+d8]    [EBX+d32]     BL       EBX
\    4     two-byte    two-byte    two-byte      AH       ESP
\    5     d32         [EBP+d8]    [EBP+d32]     CH       EBP
\    6     [ESI]       [ESI+d8]    [ESI+d32]     DH       ESI
\    7     [EDI]       [EDI+d8]    [EDI+d32]     BH       EDI
\
\  two-byte mode
\     base  mod = 0     mod = 1        mod = 2
\     ----  ----------- -------------- ---------------
\     0     [EAX+sidx]  [EAX+sidx+d8]  [EAX+sidx+d32]
\     1     [ECX+sidx]  [ECX+sidx+d8]  [ECX+sidx+d32]
\     2     [EDX+sidx]  [EDX+sidx+d8]  [EDX+sidx+d32]
\     3     [EBX+sidx]  [EBX+sidx+d8]  [EBX+sidx+d32]
\     4     [ESP+sidx]  [ESP+sidx+d8]  [ESP+sidx+d32]
\     5     [d32+sidx]  [d32+sidx+d8]  [d32+sidx+d32]
\     6     [ESI+sidx]  [ESI+sidx+d8]  [ESI+sidx+d32]
\     7     [EDI+sidx]  [EDI+sidx+d8]  [EDI+sidx+d32]
\
\
\  sidx
\     ss     scale factor      index   index register
\     0      *1                0       EAX
\     1      *2                1       ECX
\     2      *4                2       EDX
\     3      *8                3       EBX
\                              4       none
\                              5       EBP
\                              6       ESI
\                              7       EDI
\

: (OPCODE)>DIS  ( op# -- )
   OPCODE >DISLINE ;

: PARM-TAB  ( -- )
\  DisParmCol DisPos !
  Dispos @ 1+ DisParmCol max DisPos !
;

: OPCODE>DIS  ( op# -- )
   OPCODE >DISLINE  PARM-TAB ;

: op$>dis	\ caddr len -- ; SFP004
  >disline  parm-tab
;

: @BYTE>DIS  ( -- )
   @BYTE BYTE>DIS ;

: @WORD>DIS  ( -- )
   @WORD WORD>DIS ;

: @LONG>DIS  ( -- )
   @LONG LONG>DIS ;

: @xLONG>DIS  ( -- )
   @xLONG xLONG>DIS ;

: SEXT>DIS	\ byte --
  <C DUP 0<
  IF  [char] - CHAR>DIS  abs  THEN
  BYTE>DIS ;

: @DWORD  ( -- n )
   opcDsize @ 32 >=
   IF  @LONG  ELSE  @WORD  THEN ;

: DWORD>DIS  ( n -- )
  D32bit?
  IF  LONG>DIS  ELSE  WORD>DIS  THEN ;

: @DWORD>DIS  ( -- )
  @DWORD DWORD>DIS ;

CREATE StepBuff 256 ALLOT

: $stepOver	\ -- ; display and step over string at DISADR
  DisAddr StepBuff  over c@ 1+ 0 do	\ collect string
    over i + c@  over i + c!
  loop
  2drop
  bl>dis ">dis  StepBuff $>dis  ">dis	\ display string
  Disaddr count +		\ step over string (aligned)
  (DisAddr) !
;

: ZStepOver	\ -- ; display and step over string at DISADDR
  bl>dis ">dis
  DisAddr zcount  2dup >disline  + 1+ (DisAddr) !
  ">dis
;

: w$StepOver	\ -- ; display and step over string at DISADDR
  bl>dis ">dis
  DisAddr wcount  2dup >disline  + 2+ aligned (DisAddr) !
  ">dis
;

: .dest		\ -- ; display inline data as branch target
  bl>dis  @xlong xlong>dis
\  bl>dis  @long long>dis
;
REQUIRE [switch lib\Vfx\switches.fth 


[switch dasm-switch drop	\ abs24 -- ; handle special cases
  '(S")     runs $stepOver
switch]


: ShowBranchTarget      \ abs24 --
\ Show Target name of a branch address where possible.
\ ." ShowBranchTarget=" dup H.
 DUP
	NEAR_NFA 
	>R DUP
	IF S"  { " >disline  DUP COUNT >disline
	     NAME>  R> - DUP
	     IF   DUP S"  + " >disline NEGATE DWORD>DIS \ xlong>dis \ >S
	     THEN DROP        S"  }" >disline
	ELSE RDROP DROP
	THEN
\  dasm-switch				\ process special cases
  CASE
   '(S") OF $stepOver  ENDOF
  ENDCASE

 ;

: abs>dis       \ addr -- ; deal with absolute branch target address
  dup >branch-target                    \ make branch target address
  dup dword>dis                         \ generate text
  ShowBranchTarget
;

: rel>dis       \ offset -- ; generate relative branch target
  DisAddr + abs>dis			\ add, treat as absolute target
;

: abs>dis-noFwd	\ addr -- ; as ABS>DIS, but no forward branch detection
  dup dword>dis                         \ generate text
  ShowBranchTarget
;

: rel>dis-noFwd	\ offset -- ; generate relative branch target, no forward detection
  DisAddr + abs>dis-noFwd		\ add, treat as absolute target
;


: @BYTE-OFFSET  ( -- )
   @BYTE <C rel>DIS ;			\ noFwd?

: @LONG-OFFSET  ( -- )
   @DWORD
   D32bit? if <l else <w then
\   D32bit?  NOT IF <W THEN
   rel>DIS ;

: (REG)>DIS  ( n -- )
   DUP 8 >= IF
     Mode32bit?  8/16? NOT AND	\ 32 bit registers only
     16bit? NOT AND		\ 16 bit regs only set ?
     IF [char] E CHAR>DIS THEN
   THEN
   REG >DISLINE ;

: REG>DIS  ( n -- )
   bit0 wbit? AND
   \ if (w) field is in this instruction
   \ true if bit 0 is set or false if not set
   \ 16bit or 32bit data
\   16/32? OR
   16bit? OR
   IF \ 16bit and 32bit registers only
      \ instruction uses only modr/m byte
      \ for selection of registers
      8 +
   THEN
   (REG)>DIS ;

: REG32>DIS	\ n --
\ 32 bit ok if in 16 bit mode with $66 prefix
   $66? IF [char] E CHAR>DIS THEN
   8 + (REG)>DIS ;

: R/M16>DIS  ( n -- )
   R/M16 >DISLINE ;

: MEM(32bits)  ( -- )
  \ 32bit registers only for memory [reg]
  \ instruction uses only modr/m byte
  \ for selection of registers
  \ 32bit?= -1  mod= 0
  s" 0 " >disline
  [>DIS
  r/m +rex>rm areg>dis		\ SFP006
  ]>DIS
;

: SIB>DIS	\ --
  indexreg 4 <> IF
    +>DIS  indexreg +rex>index areg>dis	\ SFP006
    ss IF
      [char] * CHAR>DIS
      1 ss lshift [char] 0 + CHAR>DIS
    THEN
  THEN
;

\- SHORT? : SHORT? ( n -- -128 < n < 127 )  0x80 + 0x100 U< ;
\- LONG? : LONG? ( n -- -32768 < n < 32767 )  0x8000 + 0x10000 U< ;

: soff>dis	\ offset --
  dup SHORT? if
    dup 0<
    if  [char] - char>dis  abs  then
    byte>dis  exit
  then
  dup LONG? if
    dup 0<
    if  [char] - char>dis  abs  then
    word>dis  exit
  then
  dup 0<
  if  [char] - char>dis  abs  then
  long>dis
;

: MODRM32-2BYTE  ( -- )
  @SIB
  [>DIS
  basereg 5 <> if
    basereg +rex>base areg>dis		\ SFP006
  then
  SIB>DIS
  case  modifier
    0 of  basereg 5 =			\ no offset, except for no BASE reg
          if  +>dis @long long>dis  then
    endof
    1 of
      @BYTE ?DUP
      IF  +>DIS  SEXT>DIS THEN
    endof
    2 of
      @LONG ?DUP
      IF  +>DIS  LONG>DIS  THEN
    endof
    3 of
      1 abort" invalid modifier in MOD & SIB decode"
    endof
  endcase
  ]>DIS
;

0 value rip-dis
0 value rip-flag

: disp+rip>dis	\ -- ; display RIP addressing
  @long dup to rip-dis soff>dis  s"  [RIP] " >disline
  1 to rip-flag
;

: has-sib?	\ -- flag ; true if SIB present
  r/m 4 =  modifier 3 < AND
;


: MODRM32  ( -- )
  \ 32bit?= -1  32bit data
  \ mod= 0, 1 or 2 and r/m= 4 then sib byte present
  \ modr/m = 00 reg# 100
  r/m 4 =  modifier 3 < AND
  IF  MODRM32-2BYTE  EXIT  THEN		\ mod r/m + sib
  \ 00 101 mod= 0 & r/m= 5 data 32
  modifier 0= r/m 5 <> AND
  IF  MEM(32bits)  EXIT  THEN
  modifier 3 < IF
    modifier 0= r/m 5 = AND IF
      disp+rip>dis
    ELSE
      [>DIS
      r/m +rex>base areg>dis
      modifier 1 = IF			\ byte offset
        @BYTE ?DUP
        IF  +>DIS SEXT>DIS  THEN
      THEN
      modifier 2 = IF			\ 32 bit offset
        @LONG ?DUP
        IF  +>DIS LONG>DIS  THEN
      THEN
      ]>DIS
    THEN
  ELSE
    r/m +rex>base dreg>dis
  THEN
;


: R/M+DISP>DIS	\ -- ; SFP006
  modrm32				\ always 32/64 bit
;

: OPS>DIS	\ -- ; SFP006
  bit1 IF
    reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS
  ELSE
    R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
  THEN ;

: AC>DIS  ( --- )
  bit0 IF
    case  opcDsize @
      64 of  s" RAX"  endof
      32 of  s" EAX"  endof
        s" AX"  rot
    endcase
  ELSE
    s" AL"
  THEN
  >DISLINE ;

: IMM>DIS  ( -- )
   s" , # " >disline  bit0 IF
     @DWORD>DIS
   ELSE
     @BYTE>DIS
   THEN ;

: 2-OP-INST  ( opcode# -- )
   OPCODE>DIS
   ?8bit0				\ SFP006
   bit2 0= IF
     @MODRM OPS>DIS
   ELSE
     AC>DIS IMM>DIS
   THEN ;

: SEGREG>DIS  ( n -- )
   SEGREG >DISLINE ;

: SEGREG-OP  ( byte1 -- )
   DUP 1 AND $0A + OPCODE>DIS   \ PUSH/POP
   $18 AND -3 SCALE SEGREG>DIS ;

: M/R,SEG>DIS  ( -- )
   -1 SET-bit0		\ there is no w field in this instruction
   bit1
   IF reg# SEGREG>DIS  ,>DIS  R/M+DISP>DIS
   ELSE
      R/M+DISP>DIS  ,>DIS  reg# SEGREG>DIS
   THEN ;

: dis-reg,r/m	\ --
  @MODRM
  reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS
;

: REG,R/M*IMM	\ --
   dis-reg,r/m  s" , # " >disline
   case  opcDsize @
       8 of  @Byte Sext>Dis  endof
     16 of  @word word>dis  endof
     32 of  @long long>dis  endof
     64 of  @long long>dis  endof
   endcase
;

: ?b0->Dsize8	\ opcode -- ; 8 bit of bit0=0
  1 and 0= if  8 opcDsize !  then
;

: B/W/D/X>DIS	\ -- ; show data size
   case  opcDsize @
     64 of  [char] Q  endof
     32 of  [char] D  endof
     16 of  [char] W  endof
     8  of  [char] B  endof
   endcase
   CHAR>DIS ;

: DADJ-INST         ( byte#1 --- )
   $0C bit3 IF 1+ THEN OPCODE>DIS ; \ DAS/DAA

: ADJ-INST         ( byte#1 --- )
   $0E bit3 IF 1+ THEN OPCODE>DIS ; \ AAS/AAA

: SEG:         ( byte#1 --- )
   \ 3F MNEM>BUF ( SEG:)
   $18 AND -3 SCALE SEGREG>DIS :>DIS ;

: ILLEGAL  ( -- )
   $72 OPCODE>DIS ;

VARIABLE (FORWARD)

: (DISASM-NEXT) ( -- )
    \ Continue disassembling next instruction on current line
    \ starting at DisAddr in current mode.
    \ Forward reference used by prefices, e.g. $66 (operand size)
    (FORWARD) @ EXECUTE ;



\ Switch chains used to access extended instructions such as SSE

: illsw		( opc -- ) drop  illegal  ;
\ executed if opc not in switch chain.

[switch switch0Fxx   illsw	switch]	\ -- ; process [REX] $0F xx
[switch switch660Fxx illsw	switch]	\ -- ; process $66 [REX] $0F xx
[switch switchF20Fxx illsw	switch]	\ -- ; process $F2 [REX] $0F xx
[switch switchF30Fxx illsw	switch]	\ -- ; process $F3 [REX] $0F xx



\ --- OPCODES GROUPS DETERMINED BY MODRM

: GROUP01  ( -- )
   \ reg    opcode   op#
   \                               mod reg r/m
   \  00    ADD      00   000000dw 00  000 000  regs to regs
   \                      0000000w 00  000 000  regs to mem
   \                      0000001w 00  000 000  mem to regs
   \                      100000sw 00      000  imm to mem/regs
   \                      0000010w              imm to ac
   \  01    OR       01
   \  02    ADC      02
   \  03    SBB      03
   \  04    AND      04
   \  05    SUB      05
   \  06    XOR      06
   \  07    CMP      07
   \ s - immediate data field must be extended
   \ w - byte or full size
   \ ws = 00 byte - 01 full - 10 illegal - 11 byte xtend full
   \ all                   imm to reg/mem
   +wbit  ?8bit0			\ check byte operation
   @MODRM
   \ fetch opcode $ instruction and store in DisLine$
   reg# OPCODE>DIS
   PTR? IF  PTR>DIS  THEN
   R/M+DISP>DIS  s" , # " >disline
   bit0 bit1 NOT AND IF
     @DWORD>DIS
   ELSE
     @BYTE SEXT>DIS
   THEN ;

: GROUP02  ( byte -- )
   \ reg    opcode   op#
   \  00    ROL      59
   \  01    ROR      5A
   \  02    RCL      5B
   \  03    RCR      5C
   \  04    SHL      5D
   \  05    SHR      5E
   \  06    ???
   \  07    SAR      5F
   \ w - byte or full size
   \ byte=Cx               reg/mem by imm count
   \ byte=Dx bit1=0        reg/mem by 1
   \ byte=Dx bit1=1        reg/mem by cl
   ?8bit0
   @MODRM
   reg# 6 = IF    \ @MODRM inc DisAddr
      DEC-DisAddr ILLEGAL  DROP  EXIT
   THEN
   reg# DUP 7 = IF 1- THEN
   $59 + OPCODE>DIS
   PTR? IF  PTR>DIS  THEN
   R/M+DISP>DIS ,>DIS
   $F0 AND $C0 = IF
     s" # " >disline @BYTE>DIS
   ELSE
     bit1
     IF  s" CL"	 ELSE  s" # 1"  THEN  >disline
   THEN ;

: GROUP06  ( -- )
   \ reg    opcode   op#
   \  00    SLDT     65 00001111 00000000 mod 000 r/m
   \        SLDT     store local descriptor table
   \        SLDT     reg16 | mem16
   \  01    STR      66 00001111 00000000 mod 001 r/m
   \        STR      store task register
   \        STR      reg16  | mem16
   \  01    LTR      68 00001111 00000000 mod 001 r/m
   \        LTR      load task register
   \        LTR      reg16 | mem16
   \  02    LLDT     67 00001111 00000000 mod 010 r/m
   \        LLDT     reg16 | mem16
   \  04    VERR     69 00001111 00000000 mod 100 r/m
   \        VERR     reg16 | mem16
   \  05    VERW     6A 00001111 00000000 mod 101 r/m
   \        VERW     reg16 | mem16
   \  06-07 ???
   @MODRM
   -wbit        \ no (w) operand length field in instruction
   SET-16bit?       \ 16bit regs & mem allowed
   reg#
   CASE
    00 OF $65 >R  ENDOF
    01 OF $66 >R  ENDOF
    02 OF $4C >R  ENDOF
    03 OF $4E >R  ENDOF
    04 OF $51 >R  ENDOF
    05 OF $55 >R  ENDOF
    06 OF $72 >R  ENDOF
    07 OF $72 >R  ENDOF
   ENDCASE
   R> DUP OPCODE>DIS
   $72 =
   IF   DEC-DisAddr
   ELSE R/M+DISP>DIS
   THEN ;

: GROUP07  ( -- )
   \ reg    opcode   op#
   \  00    SGDT     6B  00001111 00000001 mod 000 r/m
   \        SGDT     mem
   \  01    SIDT     6C  00001111 00000001 mod 001 r/m
   \        SIDT     mem
   \  02    LGDT     6D  00001111 00000001 mod 010 r/m
   \  03    LIDT     6E  00001111 00000001 mod 011 r/m
   \  04    SMSW     6F  00001111 00000001 mod 100 r/m
   \  05    ???
   \  06    LMSW     70  00001111 00000001 mod 110 r/m
   \  07    ???
   @MODRM
   -wbit        \ no (w) operand length field in instruction
   reg#
   CASE
    00 OF $6B >R  ENDOF
    01 OF $6C >R  ENDOF
    02 OF $6D >R  ENDOF
    03 OF $6E >R  ENDOF
    04 OF $6F >R  ENDOF
    05 OF $72 >R  ENDOF
    06 OF $70 >R  ENDOF
    07 OF $72 >R  ENDOF
   ENDCASE
   R> DUP OPCODE>DIS
   $72 =
   IF   DEC-DisAddr
   ELSE R/M+DISP>DIS
   THEN ;


: GROUP08  ( -- )
   \ reg    opcode   op#
   \  00-03 ???
   \  04    BT       4C  00001111 10111010 mod 100 r/m imm8
   \  05    BTS      4E  00001111 10111010 mod 101 r/m imm8
   \  06    BTR      51  00001111 10111010 mod 110 r/m imm8
   \  07    BTC      55  00001111 10111010 mod 111 r/m imm8
   @MODRM  reg# 4 < IF
     DEC-DisAddr  ILLEGAL    \ @MODRM inc DisAddr
   ELSE
     set-16/32/64
     -wbit     \ no (w) operand length field in instructio
     CASE  reg#
       04 OF $4C >R  ENDOF
       05 OF $4E >R  ENDOF
       06 OF $51 >R  ENDOF
       07 OF $55 >R  ENDOF
     ENDCASE
     R> OPCODE>DIS
     PTR? IF PTR>DIS THEN
     R/M+DISP>DIS  ,>DIS  @BYTE>DIS
   THEN ;

\ --- TWO-BYTE OPCODES

: O200  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    GR06
   \  01    GR07
   \  02    LAR      48
   \  03    LSL      49
   \  04    ???
   \  05    SYSCALL
   \  06    CLTS     50
   \  07-0F ???
   $0F AND
   DUP 0=
   IF DROP GROUP06
      EXIT
   THEN
   DUP 1 =
   IF DROP GROUP07
      EXIT
   THEN
   DUP 2 3 EITHER=
   IF 2 - $48 + OPCODE>DIS
      @MODRM OPS>DIS
      EXIT
   THEN
  dup $05 = if
    drop s" SYSCALL" op$>dis  exit
  then
   dup 6 = IF				\ SFP004
     drop  $4A OPCODE>DIS exit
   THEN
   dup 8 =
   if  drop  s" INVD" op$>dis exit  THEN
   dup 9 =
   if  drop  s" WBINVD" op$>dis exit  THEN
   DROP  ILLEGAL
;

: O220  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    MOV      1A
   \  01    MOV      1A
   \  02    MOV      1A
   \  03    MOV      1A
   \  04    MOV      1A
   \  05    ???
   \  06    MOV      1A
   \  07-0F ???
   DROP
   ILLEGAL ;

: 3ops0F38xx	\ --
  case  @byte
    $f0 of  s" MOVBE" op$>dis  @modrm
            reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS
        endof
    $f1 of  s" MOVBE" op$>dis  @modrm
            R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
        endof
      illegal
  endcase
;

: O230		\ byte --
\   0F 34	\ SYSENTER
\   0F 38 F0	\ MOVBE
\   0F 38 F1	\ MOVBE
  dup $34 = if
    drop s" SYSENTER" op$>dis  exit
  then
  dup $38 = if
    drop  3ops0F38xx  exit
  then
  drop illegal
;

: O240	    \ byte -- ; SFP006
  s" CMOV" >disline
  $0F and condCode op$>dis
  -wbit  set-16/32/64
  @MODRM
  reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS
;

: O270		\ byte --
\   0F 77	\ EMMS
  dup $77 = if
    drop s" EMMS" op$>dis  exit
  then
  drop illegal
;

: O280  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    JO       17
   \  01    JNO
   \  02    JB
   \  03    JNB
   \  04    JZ
   \  05    JNZ
   \  06    JBE
   \  07    JNBE
   \  08    JS
   \  09    JNS
   \  0A    JP
   \  0B    JNP
   \  0C    JL
   \  0D    JNL
   \  0E    JLE
   \  0F    JNLE
   $17 (OPCODE)>DIS
   $0F AND CONDCODE op$>dis
   @LONG-OFFSET ;

: O290  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    SETO     4B
   \  01    SETNO
   \  02    SETB
   \  03    SETNB
   \  04    SETZ
   \  05    SETNZ
   \  06    SETBE
   \  07    SETNBE
   \  08    SETS
   \  09    SETNS
   \  0A    SETP
   \  0B    SETNP
   \  0C    SETL
   \  0D    SETNL
   \  0E    SETLE
   \  0F    SETNLE
   $4B (OPCODE)>DIS
   $0F AND CONDCODE >DISLINE
    -wbit     \ no (w) operand length field in instruction
   8 opcDsize !				\ force 8 bit data ; SFP006
\   SET-8bit?      \ set 8bit regs and memory only
   47 DisPos !
   @MODRM
   PTR?  IF PTR>DIS THEN
   R/M+DISP>DIS ;

: O2A0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    PUSH FS  0A
   \  01    POP FS   0B
   \  02    ???
   \  03    BT       4C  00001111 10100011 mod reg r/m
   \  04    SHLD     4D
   \  05    SHLD     4D
   \  06-07 ???
   \  08    PUSH GS  0A
   \  09    POP GS   0B
   \  0A    ???
   \  0B    BTS      4E  00001111 10101011 mod reg r/m
   \  0C    SHRD     4F
   \  0D    SHRD     4F
   \  0E    ???
   \  0F    IMUL     14  1111011w mod 101 r/m
   $0F AND   \ 00001111 and : use only 4bits
   DUP 2 <
   IF DROP
      $0A bit0 IF 1+ THEN OPCODE>DIS
      s" FS" >DISLINE			\ SFP006
      EXIT
   THEN
   DUP 3 =
   IF \ BT = 3
      \ 00001111 10100011 mod reg r/m
      \ reg16,reg16 | reg32,reg32 | mem16,reg16 | mem32,reg32
      DROP $4C OPCODE>DIS
      @MODRM
      \ allow only 16bit & 32bit regs and mem
      set-16/32/64 ( SET-16/32? )			\ SFP006
      R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis	\ SFP006
      EXIT
   THEN
\ SFP005...
  dup $04 = if		\ SHLD r/m, reg, imm8
    drop  s" SHLD" op$>dis
    set-16/32/64 ( SET-16/32? )			\ SFP006
    @modrm  R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
    s" , # " >disline @byte>dis
    exit
  THEN
  dup $05 = if		\ SHLD r/m, reg, cl
    drop  s" SHLD" op$>dis
    set-16/32/64 ( SET-16/32? )			\ SFP006
    @modrm  R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
    s" , CL" >DISLINE
    exit
  THEN
\ ...SFP005
   DUP 08 09 EITHER=  \ PUSH & POP GS regs
   IF DROP
      $0A bit0 IF 1+ THEN OPCODE>DIS
      s" GS" >DISLINE			\ SFP006
      EXIT
   THEN
   DUP $0B =
   IF \ BTS = 0B
      \ 00001111 10101011 mod reg r/m
      \ reg16,reg16 | reg32,reg32 | mem16,reg16 | mem32,reg32
      DROP $4E OPCODE>DIS
      @MODRM
      \ allow only 16bit & 32bit regs and mem
      set-16/32/64 ( SET-16/32? )			\ SFP006
      R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis	\ SFP006
      EXIT
   THEN
\ SFP006...
  dup $0C = if		\ SHRD r/m, reg, imm8
    drop  s" SHRD" op$>dis
    set-16/32/64 ( SET-16/32? )			\ SFP006
    @modrm  R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
    s" , # " >disline @byte>dis
    exit
  THEN
  dup $0D = if		\ SHRD r/m, reg, cl
    drop  s" SHRD" op$>dis
    set-16/32/64 ( SET-16/32? )			\ SFP006
    @modrm  R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
    s" , CL" >DISLINE
    exit
  THEN
\ ...SFP006
\ SFP008...
  dup $0E = if		\ CLFLUSH m8
    drop
    DisAddr c@ dup $e8 = if	\ LFENCE
      drop  @byte drop   s" LFENCE" op$>dis
      exit
    then
    $f0 = if				\ MFENCE
      @byte drop  s" MFENCE" op$>dis
      exit
    then
    8 opcDsize !
    s" CLFLUSH" op$>dis
    @modrm r/m+disp>dis
    exit
  then
\ ...SFP008
   DUP $0F =
   IF \ IMUL = 0F
      \ 1111011w mod 101 r/m
      \ reg16,reg16 | reg32,reg32 | reg16,mem16 | reg32,mem32
      DROP $14 OPCODE>DIS
      @MODRM
      \ allow only 16bit & 32bit regs and mem
      set-16/32/64 ( SET-16/32? )			\ SFP006
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DROP ILLEGAL ;

: O2B0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-01 ???
   \  02    LSS      50
   \  03    BTR      51  00001111 10110011 mod reg r/m
   \  04    LFS      52
   \  05    LGS      53
   \  06-07 MOVZX    54   00001111 1011011w mod reg r/m
   \  08-09 ???
   \  0A    GR08
   \  0B    BTC      55   00001111 10111011 mod reg r/m
   \  0C    BSF      56   00001111 10111100 mod reg r/m
   \  0D    BSR      57   00001111 10111101 mod reg r/m
   \  0E-0F MOVSX    58   00001111 1011111w mod reg r/m
   $0F AND   \ 00001111 and : use only 4bits
   dup 0 1 either= if			\ SFP004
     0= if  8 opcDsize !  then
     s" CMPXCHG" op$>dis		\ SFP010
     @MODRM
     R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis
     exit
   THEN
   DUP 2 =
   IF \ 02 = LSS
      \ 00001111 10110011 mod reg r/m
      2- $50 + OPCODE>DIS
      @MODRM
      \ allow only 16bit & 32bit regs and mem
      set-16/32/64 ( SET-16/32? )	\ SFP006
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DUP 3 =
   IF \ 03 = BTR
      \ 00001111 10110011 mod reg r/m
      DROP $51 OPCODE>DIS
      @MODRM
      \ allow only 16bit & 32bit regs and mem
      set-16/32/64 ( SET-16/32? )			\ SFP006
       R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis	\ SFP006
      EXIT
   THEN
   DUP 4 5 EITHER=
   IF \ LGS = 5 & LFS = 4
      \ LFS = 00001111 1011100 mod reg r/m
      \ LGS = 00001111 1011101 mod reg r/m
      2- $50 + OPCODE>DIS
      \ no (w) operand length field in this instruction
       -wbit
      set-16/32/64 ( SET-16/32? )			\ SFP006
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DUP 6 7 EITHER=              \ MOVZX = 6 or 7
   OVER $0E $0F EITHER= OR IF   \ MOVSX = 0E or 0F
\ *** FIX DATA SIZE STUFF HERE
      dup 1 and 0= >r		\ 8 bit src unit
      6 7 EITHER=               \ MOVZX = 6 or 7
      IF $54 ELSE $58 THEN
      OPCODE>DIS
      \ MOVZX = $54
      \ MOVZX = 6 or 7
      \ 00001111 | 1011011w | mod reg r/m
      \ MOVSX = $58
      \ MOVSX = 0E or 0F
      \ 00001111 | 1011111w | mod reg r/m
\      set-16/32/64 ( SET-16/32? )	\ SFP006
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS	\ SFP006
\      ZERO-16/32?   \ source can 8bit or 16bit
      SET-8/16?      \ set 8bit & 16bit regs only
      r> if  8 opcDsize !  else  16 opcDsize !  then
      PTR? IF  PTR>DIS  THEN
      R/M+DISP>DIS
      EXIT
   THEN
   DUP $0A =
   IF \ 0A = GROUP08
      DROP GROUP08
      EXIT
   THEN
   DUP $0B =
   IF \ 0B = BTC
      \ 00001111 10111011 mod reg r/m
      \ reg16,reg16 | reg32,reg32 | mem16,reg16 | mem32,reg32
      $0B - $55 + OPCODE>DIS
      @MODRM
      set-16/32/64 ( SET-16/32? )			\ SFP006
      \ allow only 16bit & 32bit registers and memory
      R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis	\ SFP006
      EXIT
   THEN
   DUP $0C $0D EITHER= IF		\  0C = BSF , 0D = BSR
     $0B - $55 + OPCODE>DIS
     set-16/32/64  @MODRM		\ SFP006
     reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
     EXIT
   THEN
   DROP ILLEGAL ;

: O2C0		\ byte -- ; SFP004
  $0F and
  dup 8 >= if				\ BSWAP
    s" BSWAP" op$>dis  7 and +rex>reg dreg>dis
    exit
  THEN
  drop illegal
;

: O2??  ( byte -- )
   DROP ILLEGAL ;

CREATE OP2map	\ -- addr ; SFP006
  ' O200 ,	' O2?? ,	' O220 ,	' O230 ,
  ' O240 ,	' O2?? ,	' O2?? ,	' O270 ,
  ' O280 ,	' O290 ,	' O2A0 ,	' O2B0 ,
  ' O2C0 ,	' O2?? ,	' O2?? ,	' O2?? ,

: OP2  ( byte2 -- )
   DUP $F0 AND $10 / cells OP2map + @ EXECUTE ;

: OP0F  ( -- )
   @OPCODE DUP Opcode2 !   \ save second byte opcode
   OP2 ;

DEFER dodoRex

: [Rex]+$0f?	\ prefix -- flag
\ DISADDR points after the prefix byte.
\ Return true if the byte sequence is:
\   prefix 0F  or  prefix [REX] 0F
\ If true, DISADDR is moved, otherwise it is left alone.
  disAddr dup >r c@ $F0 and $40 = if	\ REX?
    disAddr 1+ c@ $0F = if 			\ F3 REX 0F
      prefix-byte !  disAddr c@ dodoRex
      r> drop  @byte drop  @byte drop  true  exit
    then
  then
  disAddr c@ $0F = if			\ F3 0F
    prefix-byte !
    r> drop  @byte drop  true  exit
  then
  r> set-DisAddr
  drop  false
;

: ?exec-pre[REX]0F	\ prefix switchxt -- flag
\ If the opcode sequence (prefix already fetched) matches
\   <prefix> REX 0F xx
\   <prefix> 0F xx
\ and xx is in the switch chain, then execute the switch chain.
  { prefix switchxt -- :}
  DisAddr >r  prefix [Rex]+$0f? if		\ first check for 0F or REX 0F
    DisAddr c@ switchxt inSwitch? if	\ and it is handled by hdasmSSE.fth
      r> drop
      @byte switchxt execute			\ go to it
      true  exit
    then
  then
  r> set-DisAddr  false
;

\ --- ONE BYTE OPCODES

: OP00  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-05 ADD      00
   \  ADD 000000dw mod reg r/m  regs > regs
   \      0000000w mod reg r/m  regs > mem
   \      0000001w mod reg r/m   mem > regs
   \  06    PUSH ES  0A
   \  07    POP ES   0B
   \  08-0D OR       01
   \  0E    PUSH CS  0A
   \  0F    OP0F
   DUP $0F = IF
     drop
     DisAddr c@ ['] switch0Fxx inSwitch? if \ and it is handled by hdasmSSE.fth
       @byte switch0Fxx  exit	\ go to it
     then
     OP0F
   ELSE
      DUP 6 AND 6 =
      IF SEGREG-OP
      ELSE
         $38 AND -3 SCALE 2-OP-INST
      THEN
   THEN ;

: OP10  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-05 ADC      02
   \  06    PUSH SS  0A
   \  07    POP SS   0B
   \  08-0D SBB      03
   \  0E    PUSH DS  0A
   \  0F    POP DS   0B
   DUP 6 AND 6 =
   IF SEGREG-OP
   ELSE
      $38 AND -3 SCALE 2-OP-INST
   THEN ;

: OP20  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-05 AND      04
   \  06    SEG ES
   \  07    DAA      0C
   \  08-0D SUB      05
   \  0E    SEG CS
   \  0F    DAS      0D
   DUP $18 OR $3E =
   IF SEG:
   ELSE
      DUP 7 AND 7 =
      IF DADJ-INST DROP
      ELSE
         $38 AND -3 SCALE 2-OP-INST
      THEN
   THEN  ;

: OP30  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-05 XOR      06
   \  06    SEG SS
   \  07    AAA      0E
   \  08-0D CMP      07
   \  0E    SEG CS
   \  0F    AAS      0F
   DUP $18 OR $3E =
   IF SEG:
   ELSE
      DUP 7 AND 7 =
      IF ADJ-INST DROP
      ELSE
         $38 AND -3 SCALE 2-OP-INST
      THEN
   THEN  ;

: doRex		\ byte --
  rex-byte !  rex.w?			\ check for 64 bit data
  if  64 opcDsize !  then
;

' doRex TO dodoRex

: OP40		\ byte -- ; REX = 40..4F
  doRex (DISASM-NEXT)
;

: OP50  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-07 PUSH     0A
   \  08-0F POP      0B
   DUP $58 <
   IF $0A ELSE $0B THEN OPCODE>DIS
   opcDsize @ 16 <>
   if  64 opcDsize !  then
   7 and  rex.b? if  8 +  then dreg>dis
\   7 AND REG32>DIS
;

: .size16/64	\ -- ; display data size 16/64
  opcDsize @ 16 <>
  if  [char] Q char>dis  then
  s" WORD " >DISLINE
;
CREATE $NOP10 9 C, $2e C, $0f C, $1f C, $84 C, 0 C,  0 L,

: OP60  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    PUSHA    10
   \  01    POPA     11
   \  02    BOUND    12    reg,mem
   \  03    MOVSXD         reg32/64, reg/mem32
   \  04    SEG FS
   \  05    SEG GS
   \  06    op size  66    32bit to 16bit data overide
   \  07    adr size 67
   \  08    PUSH     0A    imm full 011010s0 imm data
   \  09    IMUL     14
   \ IMUL reg,reg/mem*imm sext 00001111 10101111 mod reg r/m
   \  0A    PUSH     0A    imm byte 011010s0 imm data
   \  0B    IMUL     14
   \ IMUL reg,reg/mem*imm byte 011010s1 mod reg r/m imm
   \  0C    INSB     15
   \  0D    INSW/D   15
   \  0E    OUTSB    16
   \  0F    OUTSW/D  16
   $0F AND
   DUP 3 <
   IF DUP $10 + OPCODE>DIS
      s" Invalid in 64 bit mode" >disline
      EXIT
   THEN
   dup 3 = if
     drop  s" MOVSXD" op$>dis
     @MODRM  reg# +rex>reg dreg>dis
     ,>DIS  32 opcDsize !  ptr? if PTR>DIS then  R/M+DISP>DIS
     exit
   then
   \ - FS: GS:
   DUP 4 5 EITHER=
   IF SEGREG>DIS :>DIS
      EXIT
   THEN
\ SFP006...
   \ -- operand size override, or 66 0F xx, or 66 REX 0F
   DUP 6 = IF
     drop $66 ['] switch660Fxx ?exec-pre[REX]0F	\ check 660Fxx sequence
     if  exit  then
	$NOP10  COUNT \     s\" \x2e\x0f\x1f\x84\x00\x00\x00\x00\x00"
	tops=	\ 662e0f1f840000000000
     if  s" NOP10" >disline  exit  then
     opcDsize @ 32 >=			\ 32/64 -> 16, 16 -> 32
     if  16  else  32  then
     opcDsize !
     ($66?) on  (DISASM-NEXT)  EXIT
   THEN
   \ -- address size override
   DUP 7 = IF
     DROP  opcAsize @ 32 >=		\ 32/64 -> 16, 16 -> 32
     if  16  else  32  then
     opcAsize !
     ($67?) on  (DISASM-NEXT)  EXIT
   THEN
\ ...SFP006
   \ - PUSH imm
   DUP 8 $0A EITHER=
   IF DROP $0A OPCODE>DIS
      bit1
      IF .size16/64 @BYTE>DIS
      ELSE
         .size16/64  opcDsize @ 16 =
         if  @WORD>DIS  else  @DWORD>DIS  then
      THEN
      EXIT
   THEN
   \ - IMUL reg,reg/mem*imm
   DUP 09 = IF
     DROP  -wbit  set-16/32/64  ( SET-16/32? )	\ only 16bit & 32bit regs allowed ; SFP006
     $14 OPCODE>DIS
     REG,R/M*IMM
     EXIT
   THEN
   DUP $0B =
   IF \ IMUL = 0B
      \ reg16,imm8 | reg32,imm8 | mem32,imm8
      DROP  $14 OPCODE>DIS
       -wbit
      \ no (w) operand length field in this instruction
      set-16/32/64 ( SET-16/32? )	\ SFP006
      @MODRM
      reg# +rex>reg dreg>dis  ,>dis  R/M+DISP>DIS
       s" , # " >disline  @BYTE SEXT>DIS
      EXIT
   THEN
   \ - INS/OUTS
   $0C - 2/ $15 + OPCODE >DISLINE
   bit0 IF
     case  opcDsize @
       64 of  [char] Q  endof
       32 of  [char] D  endof
         [char] W  swap
     endcase
   ELSE  [char] B  THEN
   CHAR>DIS ;

: OP70  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    JO       17
   \  01    JNO
   \  02    JB
   \  03    JNB
   \  04    JZ
   \  05    JNZ
   \  06    JBE
   \  07    JNBE
   \  08    JS
   \  09    JNS
   \  0A    JP
   \  0B    JNP
   \  0C    JL
   \  0D    JNL
   \  0E    JLE
   \  0F    JNLE
   $17 (OPCODE)>DIS
   $0F AND CONDCODE op$>dis
   @BYTE-OFFSET ;

: OP80  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-01 GR01
   \  02    ???
   \  03    GR01
   \  04-05 TEST     18 1000010w mod reg r/m
   \ TEST = 1111011w mod 000 r/m
   \  06-07 XCHG     19
   \  08-0B MOV      1A
   \  0C    MOV ..   1A
   \  0D    LEA      1B  10001101 mod reg r/m
   \  0E    MOV      1A
   \  0F    POP      0B
   $0F AND
   DUP 4 < IF
     DROP  GROUP01  EXIT
   THEN
   DUP 4 5 EITHER=
   IF \ TEST = 04-05
      \ TEST opcode = 18
      \ TEST
      4 - 2/ $18 + OPCODE>DIS
      $66? 0= if  ?8bit0  then
      @MODRM
      R/M+DISP>DIS  ,>DIS  reg# +rex>reg dreg>dis	\ SFP006
      EXIT
   THEN
   DUP 6 7 EITHER=
   IF \ XCHG = 06-07
      \ XCHG 1000011w mod reg r/m
      4 - 2/ $18 + OPCODE>DIS
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DUP 8 $0A 1+ WITHIN
   IF $08 $0A either= if  8 opcDsize !  THEN
      $1A OPCODE>DIS
      @MODRM OPS>DIS
      EXIT
   THEN
   DUP $0B =
   IF \ MOV = 0B
      \ reg16,reg16 | reg16,mem16 | reg32,reg32 | reg32,mem32
      \ 1000101w mod reg r/m
      set-16/32/64 ( SET-16/32? )	\ SFP006
      DROP $1A OPCODE>DIS
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DUP $0C =
   IF \ MOV = 0C
      \ reg16,sreg | mem16,sreg
      \ 1000101w mod reg r/m
      set-16/32/64
      DROP $1A OPCODE>DIS
      @MODRM
      R/M+DISP>DIS  ,>DIS  reg# SEGREG>DIS
      EXIT
   THEN
   DUP $0E =
   IF \ MOV = 0E
      \ sreg,reg16 | sreg,mem16
      \ 10001100 mod sreg r/m
      drop
      -wbit			\ no (w) operand length field in this instruction
      set-16/32/64
      $1A OPCODE>DIS
      @MODRM  reg# SEGREG>DIS  ,>DIS  R/M+DISP>DIS
      EXIT
   THEN
   $0D =
   IF \ LEA = 0D
      \ 10001101 mod reg r/m
      -wbit
      \ no (w) operand length field in this instruction
      set-16/32/64 ( SET-16/32? )	\ SFP006
      $1B OPCODE>DIS
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
   ELSE
      \ POP = 0F
      \ POP 10001111 mod 000 r/m
      \ mem16 : mem32
      -wbit
      \ no (w) operand length field in this instruction
      set-16/32/64 ( SET-16/32? )	\ SFP006
      $0B OPCODE>DIS
      @MODRM  ptr? if PTR>DIS then  R/M+DISP>DIS
   THEN ;

: OP90  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    NOP      1C
   \  01-07 XCHG     19
   \  08    CBW      1D
   \  09    CWD      1E
   \  0A    CALL     1F
   \  0B    WAIT     20
   \  0C    PUSHF    21
   \  0D    POPF     22
   \  0E    SAHF     23
   \  0F    LAHF     24
   $0F AND
   DUP 0=
   IF DROP $1C OPCODE>DIS
      EXIT
   THEN
   DUP 8 <
   IF $19 OPCODE>DIS
      7 and >r  0 dreg>dis  ,>dis  r> +rex>rm dreg>dis  exit
\      7 AND 0 REG32>DIS ,>DIS REG32>DIS
\      EXIT
   THEN
   DUP $0A =
   IF $1F OPCODE>DIS
      @DWORD @WORD>DIS :>DIS DWORD>DIS
   THEN
   dup $08 = if				\ CBW and friends
     drop
     case  opcDsize @
       16 of  s" CBW" >disline  endof
       32 of  s" CWDE" >disline  endof
       64 of  s" CDQE" >disline  endof
         s" ???" >disline
     endcase
     exit
   then
   dup $09 = if				\ CWD and friends
     drop
     case  opcDsize @
       16 of  s" CWD" >disline  endof
       32 of  s" CDQ" >disline  endof
       64 of  s" CQO" >disline  endof
         s" ???" >disline
     endcase
     exit
   then
   DUP $0B =  \ WAIT = 0B
   IF DROP $20 OPCODE>DIS EXIT THEN
   DUP $0C =  \ PUSHF = 0C
   IF DROP $21 OPCODE>DIS EXIT THEN
   DUP $0D =  \ POPF = 0D
   IF DROP $22 OPCODE>DIS EXIT THEN
   DUP $0E =  \ SAHF = 0E
   IF DROP $23 OPCODE>DIS EXIT THEN
   DUP $0F =  \ LAHF = 0F
   IF DROP $24 OPCODE>DIS EXIT THEN
   7 AND
   D32bit?
   IF   IF $71 ELSE $73 THEN
   ELSE $1D +
   THEN
   OPCODE>DIS ;

: @XWORD>DIS	\ -- ; display 64 bit item
  @long @long long>dis [char] : char>dis long>dis
;

: OPA0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-03 MOV      1A
   \  04    MOVSB    25
   \  05    MOVSW/D  25
   \  06    CMPSB    26
   \  07    CMPSW/D  26
   \  08-09 TEST     18
   \  0A    STOSB    27
   \  0B    STOSW/D  27
   \  0C    LODSB    28
   \  0D    LODSW/D  28
   \  0E    SCASB    29
   \  0F    SCASW/D  29
   $0F AND
   DUP 4 < IF
     DROP $1A OPCODE>DIS ( MOV)
     ?8bit0  bit1 IF
       [>DIS @XWORD>DIS ]>DIS ,>DIS AC>DIS
     ELSE
       AC>DIS ,>DIS [>DIS @XWORD>DIS ]>DIS
     THEN
     EXIT
   THEN
   DUP 4 7 1+ WITHIN IF
     dup ?b0->Dsize8
      4 - 2/ $25 + (OPCODE)>DIS ( MOVS/CMPS)
      B/W/D/X>DIS
      EXIT
   THEN
   DUP 8 9 EITHER=
   IF DROP $18 OPCODE>DIS
      ac>dis imm>dis  exit
(*
      AC>DIS ,>DIS bit0 ( TEST)
      IF @DWORD>DIS			\ ??????? 8/16/32/64
      ELSE
         @BYTE>DIS
      THEN
      EXIT
*)
   THEN
   dup ?b0->Dsize8
   $0A - 2/ $27 + (OPCODE)>DIS ( STOS/LODS/SCAS)
   B/W/D/X>DIS ;

: imm8/16/32/64>dis	\ -- ; SFP008
  case  opcDsize @
    64 of  @xword>dis  endof
    32 of  @long>dis  endof
    16 of  @word>dis  endof
    8 of  @byte>dis  endof
  endcase
;

: OPB0		\ byte -- ; SFP008
   \ bit0-3 opcode   op#
   \  00-07 MOV byte 1A
   \  08-0F MOV w/dw 1A
   $1A OPCODE>DIS
   bit3 0= if  8 opcDsize !  then
   7 and  rex.b? if  8 +  then
   dreg>dis s" , # " >disline
   imm8/16/32/64>dis
;

: OPC0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-01 GR02
   \  02-03 RET near 2A
   \  04    LES      2B   11000100 mod reg r/m
   \  05    LDS      2C   11000101 mod reg r/m
   \  06-07 MOV      1A
   \  08    ENTER    2D
   \  09    LEAVE    2E
   \  0A-0B RET far  2A
   \  0C    INT 3    2F
   \  0D    INT n    2F
   \  0E    INTO     30
   \  0F    IRET     31
   DUP $C2 <
   IF GROUP02
      EXIT
   THEN
   $0F AND
   bit2 NOT bit1 AND
   IF DROP $2A (OPCODE)>DIS bit3 ( RET)
      IF [char] F CHAR>DIS THEN
      bit0 NOT
      IF PARM-TAB s" # " @WORD>DIS THEN
      EXIT
   THEN
   DUP 4 5 EITHER=
   IF \ LDS = 5 & LES = 4
      \ LES = 11000100 mod reg r/m
      \ LDS = 11000101 mod reg r/m
      DROP bit0
      IF $2C ELSE $2B THEN
       -wbit
      \ no (w) operand length field in this instruction
      set-16/32/64 ( SET-16/32? )	\ SFP006
      OPCODE>DIS ( LDS/LES)
      @MODRM
      reg# +rex>reg dreg>dis  ,>DIS  R/M+DISP>DIS	\ SFP006
      EXIT
   THEN
   DUP 6 7 EITHER= IF
      6 = if  8 opcDsize !  then
      $1A OPCODE>DIS  ( MOV )
      @MODRM ptr? if PTR>DIS then R/M+DISP>DIS IMM>DIS
      EXIT
   THEN
   DUP 8 9 EITHER=
   IF 8 - $2D + OPCODE>DIS
      bit0 NOT
      IF @WORD>DIS ,>DIS @BYTE>DIS
      THEN
      EXIT
   THEN
   DUP $0C $0D EITHER=
   IF DROP $2F OPCODE>DIS bit0 ( INT)
      IF @BYTE>DIS ELSE [char] 3 CHAR>DIS THEN
      EXIT
   THEN
   DROP bit0
   IF $31 ELSE $30 THEN OPCODE>DIS ; ( IRET/INTO)

: OPD0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00-03 GR02
   \  04    AAM      32
   \  05    AAD      33
   \  06    ???
   \  07    XLAT     34
   \  08-0F ESC coprocessor
   DUP $D4 <
   IF GROUP02
      EXIT
   THEN
   $0F AND
   DUP 4 5 EITHER=
   IF 4 - $32 + OPCODE>DIS
      EXIT
   THEN
   DUP 6 =
   IF ILLEGAL DROP
      EXIT
   THEN
   DUP 7 =
   IF DROP $34 OPCODE>DIS ( XLAT)
      EXIT
   THEN
   DROP
   NDPins? 0= if
     $3F OPCODE>DIS ( ESC)
     @MODRM R/M+DISP>DIS
   THEN
;


: OPE0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    LOOPNE   35
   \  01    LOOPE    36
   \  02    LOOP     37
   \  03    JCXZ     38
   \  04-05 IN       39
   \  06-07 OUT      3A
   \  08    CALL     1F
   \  09-0B JMP      3F
   \  0C-0D IN       39
   \  0E-0F OUT      3A
   $0F AND
   DUP 4 <
   IF 3 AND $35 + OPCODE>DIS @BYTE-OFFSET
      EXIT
   THEN
   bit2 ( IN and OUT )
   IF DROP bit1 ( OUT or IN )
      IF $3A ELSE $39 THEN OPCODE>DIS
      bit1 NOT IF AC>DIS ,>DIS THEN
      bit3 ( port or DX )
      IF c" DX" count >DISLINE
      ELSE
         @BYTE>DIS
      THEN
      bit1 IF ,>DIS AC>DIS THEN
      EXIT
   THEN
   DUP 8 =
   IF DROP $1F OPCODE>DIS ( CALL )
      @DWORD <l rel>DIS-noFwd
      EXIT
   THEN
   DUP 8 >
   IF $3B OPCODE>DIS ( JMP )
      3 AND
      CASE
         1 OF @DWORD <l rel>DIS                 ENDOF
         2 OF @DWORD @WORD>DIS :>DIS DWORD>DIS  ENDOF
         3 OF @BYTE-OFFSET                      ENDOF
      ENDCASE
      EXIT
   THEN ;

0 value CallAbsInd?	\ -- flag
\ Set for CALL [] addr

variable <6/7>	\ --
\ Holds bottom three bits of opcode for size checking
variable <e/f>	\ --
\ Holds bottom three bits of opcode for size checking

: OPF0  ( byte -- )
   \ bit0-3 opcode   op#
   \  00    LOCK     3C
   \  01    ???
   \  02    REPNE    3D
   \  03    REP      3E
   \  04    HLT      40
   \  05    CMC      41
   \  06-07 GR03
   \  08    CLC      42
   \  09    STC      43
   \  0A    CLI      44
   \  0B    STI      45
   \  0C    CLD      46
   \  0D    STD      47
   \  0E    GR04
   \  0F    GR05
   $0F AND
   DUP 1 =
   IF ILLEGAL DROP
      EXIT
   THEN
   dup 2 = if				\ F2
     drop $F2 ['] switchF20Fxx ?exec-pre[REX]0F
     if  exit  then
     s" REPNE" >disline
     exit
   then
   dup 3 = if				\ F3
     drop  $F3 ['] switchF30Fxx ?exec-pre[REX]0F
     if  exit  then
     s" REP" >disline
     exit
   then
   DUP 6 7 EITHER=
   IF dup <6/7> !  6 =
      if  8 opcDsize !  then
      @MODRM
      reg#  \ reg field select instruction
      CASE
         0 OF $18 >R ENDOF   \ TEST
         1 OF $72 >R ENDOF   \ ???
         2 OF $60 >R ENDOF   \ NOT
         3 OF $61 >R ENDOF   \ NEG
         4 OF $62 >R ENDOF   \ MUL
         5 OF $14 >R ENDOF   \ IMUL
         6 OF $63 >R ENDOF   \ DIV    11111011w mod 110 r/m
         7 OF $64 >R ENDOF   \ IDIV   11111011w mod 111 r/m
      ENDCASE
      R> DUP OPCODE>DIS
      PTR?  IF PTR>DIS THEN
      $72 <>
      IF R/M+DISP>DIS  reg# 0=
         IF IMM>DIS THEN
      THEN
      EXIT
   THEN
   DUP $0E $0F EITHER=
   IF dup <e/f> !
      @MODRM
      DUP  $0E = reg# 1 > AND
      SWAP $0F = reg# 7 = AND OR
      IF  ILLEGAL   EXIT  THEN
      0 TO CallAbsInd?			\ assume not CALL [] addr
      reg#
      CASE
         0 OF <e/f> @ $0E =
              if  8 opcDsize !  then
              $08 OPCODE>DIS  ( INC)
              PTR? IF  PTR>DIS  THEN
           ENDOF
         1 OF <e/f> @ $0E =
              if  8 opcDsize !  then
              $09 OPCODE>DIS  ( DEC)
              PTR?  IF PTR>DIS THEN
           ENDOF
         2 OF $1F OPCODE>DIS setD16/64 ENDOF ( CALL)
         3 OF $1F OPCODE>DIS setD16/64 ENDOF ( CALL)
         4 OF $3B OPCODE>DIS setD16/64 ENDOF ( JMP)
         5 OF $3B OPCODE>DIS setD16/64 ENDOF ( JMP)
         6 OF $0A OPCODE>DIS  ( PUSH)
              .size16/64
           ENDOF
      ENDCASE
      R/M+DISP>DIS
      EXIT
   THEN
   DUP 1 > IF 1- THEN
   DUP 2 > IF 1+ THEN
   DUP 6 > IF 2- THEN
   $3C + OPCODE>DIS ;


\ primary despatcher

CREATE OP1map	\ -- addr
  ' OP00 ,  ' OP10 ,  ' OP20 ,  ' OP30 ,  ' OP40 ,  ' OP50 ,  ' OP60 ,  ' OP70 ,
  ' OP80 ,  ' OP90 ,  ' OPA0 ,  ' OPB0 ,  ' OPC0 ,  ' OPD0 ,  ' OPE0 ,  ' OPF0 ,

: OP1  ( byte -- )
   DUP $F0 AND $10 / cells OP1map + @ EXECUTE ;

: DISASM-NEXT  ( -- )
   INIT-VARS
   @OPCODE DUP Opcode1 !   \ save opcode first byte in var
   OP1
   rip-flag if
\ cr ." DA=" DisAddr .dword  ." , RD=" rip-dis .dword  ." , DEST=" DisAddr rip-dis + .dword
     s"    @" >disline  DisAddr rip-dis + long>dis
     0 to rip-dis  0 to rip-flag
   then
;

' DISASM-NEXT (FORWARD) !

: (DISASM)  ( -- )
   BASE @ >R HEX
   ZERO-$66?				\ reset operand size overide flag
   ($67?) off				\ reset address override
   DefDsize @ opcDsize !		\ default data size
   DefAsize @ opcAsize !		\ default address size
   0 rex-byte !				\ no 64 bit prefix
   0 prefix-byte !			\ no 66, F2 or F3 prefix with 0F
   DISINIT
   DISASM-NEXT
   disclose
   R> BASE ! ;

[IFNDEF] -TRAILING
: -TRAILING  ( c_addr u1 -- c_addr u2 ) \ string dash-trailing
\ Adjust the string specified by @i{c-addr, u1} to remove all
\ trailing spaces. @i{u2} is the length of the modified string.
    BEGIN
	DUP
    WHILE
	1- 2DUP + C@ BL <>
    UNTIL  1+  THEN ;
[THEN]      

: .DISLINE  ( -- ) CR DisLine$ -TRAILING TYPE ;


EXPORT


: INST
 SET-DisAddr (DISASM) .DISLINE  DisAddr ;

: DISA ( assr - addr )
 BEGIN
   INST 
   KEY $1B =  \ Esc
 UNTIL
;

VARIABLE  COUNT-LINE

: REST          ( ADR -- )
                4    COUNT-LINE !
                DUP SET-DisAddr
                BEGIN
                        DisAddr C@
                        DUP  0xC3 <> 
                        SWAP 0xE9 <> AND    \ NEXT, BEHIND US?
                        OVER THERE - 0x100 U> AND
                WHILE   INST

                        COUNT-LINE @ 1- DUP 0=  \ SEE-KET-FL AND
                           IF 9 EMIT ." \ Press <enter> | q | any" KEY UPC
                            DUP   0xD = IF 2DROP 1  ELSE
                              DUP [CHAR] Q = SWAP 0x1B =
                              OR IF  2DROP CR EXIT    THEN
                                DROP 20    THEN

                           THEN
                        COUNT-LINE !

                REPEAT INST DROP CR ." END-CODE  "
                ;

: SEE       ( -- )
            ' REST ;

;MODULE

\ ======
\ *> ###
\ ======

