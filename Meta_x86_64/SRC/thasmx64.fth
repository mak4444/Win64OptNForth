\ hasmx64.fth - x64 Forth host assembler
(*

tel: +44 (0)23 8063 1441
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com

From North America, our telephone and fax numbers are:
  011 44 23 8063 1441


To do
=====

Change history
==============
20200807 SFP071 Corrected (XCHG)
20200612 SFP070 Corrected mod/rm, sib and REX corner cases.
20200423 SFP069 Overhaul of SIB addressing - it was wrong!
20200402 SFP068 Reordered for host assembler 386 compatibility.
20200316 SFP067 Reorganised (MOV) to improve reg,imm actions.
20190610 SFP066 Reorganised moffset recognition in (MOV).
		Corrected RIP forward referencing.
20181003 SFP065 CMPXCHG and friends.
20170404 SFP064 Started 64 bit update.

20071119 SFP003 Added PUSH 32 by using DWORD
20020221 SFP002 Finally fixed FSUB/R reg, reg opcodes.
20011229 SFP001 Fixed shift by 0 bug.

Added CPUID and RDTSC instrs.   SFP Jan 1999
Added SHORT/LONG test to (JMP)  SFP June 1998
Conversion to v6 cross assembler SFP Feb/Mar 1998
conversion to cross assembler   SFP March '94
further modifications:-         RGG January '92
conversion to 80387:-           RGG June-July '91
reorganised source order:-      RGG June '91
conversion to PowerForth/386:-  RGG April-June '91
conversion to text file:-       RGG April '91
conversion to modular forth:-   sfp may 86

REX encoding
http://sandpile.org/x86/opc_enc.htm

REX.R, REX.B, REX.W ("REX") affects mod R/M byte
http://www.sandpile.org/x86/opc_rm.htm

REX.X and REX.B affects SIB byte
http://www.sandpile.org/x86/opc_sib.htm

REX.W affects instruction operand size
http://www.sandpile.org/x86/mode.htm
*)

\ =========
\ *! asmx64
\ *T Intel/AMD x64 Assembler
\ =========
\ *P VFX Forth has a built-in assembler. This is to enable you to write
\ ** time-critical definitions - if time is a constraint - or to do
\ ** things that might perhaps be more difficult in Forth - things such
\ ** as interrupt service routines. The assembler supports the Intel/AMD64
\ ** and the stack FP coprocessor. The supported instructions are
\ ** mainly for the benefit of the code generator. In normal use,
\ ** the assembler is very rarely needed.

\ *P Definitions written in assembler may use all
\ ** the variables, constants, etc. used by the Forth system, and may
\ ** be called from the keyboard or from other words just like any
\ ** Forth high-level word. It is important when writing a code
\ ** definition to remember which machine registers are used by the
\ ** Forth system itself. These registers are documented later in this
\ ** chapter. All other registers may be used freely. The reserved
\ ** registers may also be used - but their contents must be preserved
\ ** while they are in use and reset afterwards.

\ *P The assembler mnemonics used in the Forth assembler are just the
\ ** same as those documented in the Intel literature. The operand
\ ** order is also the same. The only difference is the need for
\ ** a space between each portion of the instruction. This is a
\ ** requirement of the Forth interpreter.

\ *P The assembler has certain defaults. These cover the order of the
\ ** operands, the default addressing modes and the segment size. These
\ ** are described later in this chapter.

\ *P The assembler source code is at *\fo(Kernel/x64Com/hasmx64.fth).
\ ** Do not treat it as an example of good Forth style. The original
\ ** 8086 assembler was witten in the early 1980s, and has survived
\ ** several upgrades to come to its present form.

\ **********************
\ *S Using the assembler
\ **********************
\ *P Normally the assembler will be used to create new Forth words
\ ** written in assembler. Such words use *\fo{CODE} and
\ ** *\fo{END-CODE} in place of *\fo{:} and *\fo{;} or *\fo{CREATE}
\ ** and *\fo{;CODE} in place of *\fo{CREATE} and *\fo{DOES>}.

\ *P The word *\fo{CODE} creates a new dictionary header and enables
\ ** the assembler.

\ *P As an example, study the definition of *\fo{0<} in assembly
\ ** language. The word *\fo{0<} takes one operand from the stack
\ ** and returns a true value, -1, if the operand was less than
\ ** zero, or a false value, 0, if the operand was greater than
\ ** or equal to zero.

\ *E CODE 0< \ n - t/f ; define the word 0<
\ **   OR RBX, RBX      \ use OR to set flags
\ **   L, IF,           \ less than zero ?
\ **   IF,              \ y:
\ **     MOV RBX, # -1  \   -1 is true flag
\ **   ELSE,            \ n:
\ **     XOR RBX, RBX   \  dirty set to 0
\ **   ENDIF,
\ **   NEXT,            \ return to Forth
\ ** END-CODE

\ *P Notice how the word *\fo{NEXT,} is used. *\fo{NEXT,} is a
\ ** macro that assembles a return to the Forth inner interpreter.
\ ** All code words must end with a return to the inner interpreter.
\ ** The example also demonstrates the use of structuring words
\ ** within the assembler. These words are pre-defined macros
\ ** which implement the necessary branching instructions. The
\ ** next example shows the same word, but implemented using local
\ ** labels instead of assembler structures for the control
\ ** structures.

\ *E CODE 0<         \ n - t/f ; define the word 0<
\ **   OR RBX, RBX   \ use OR to set flags
\ **   JGE L$1       \ skip if AX>=0
\ **   MOV RBX, # -1 \ -1 is true flag
\ **   JMP L$2       \ this part done
\ ** L$1:            \ do following otherwise
\ **   SUB RBX, RBX  \ dirty set to 0
\ ** L$2:
\ **   NEXT,         \ return to Forth
\ ** END-CODE

\ ****************************
\ *S Assembler extension words
\ ****************************
\ *P There are several useful words provided within VFX Forth to control
\ ** the use of the assembler.

\ *C ;code      \ --
\ *P Used in the form:
\ *C   : <namex> CREATE .... ;CODE ... END-CODE
\ *P Stops compilation, and enables the assembler. This word is
\ ** used with *\fo{CREATE} to produce defining words whose run-time
\ ** portion is written in code, in the same way that *\fo{CREATE ... DOES>}
\ ** is used to create high level defining words.

\ *P The data structure is defined between *\fo{CREATE} and
\ ** *\fo{;CODE} and the run-time action is defined between
\ ** *\fo{;CODE} and *\fo{END-CODE}. The current value of the
\ ** data stack pointer is saved by *\fo{;CODE} for later use by
\ ** *\fo{END-CODE} for error checking.

\ *P When *\fo{<namex>} executes the address of the data area will
\ ** be the top item of the CPU call stack. You can get the address
\ ** of the data area by *\fo{POP}ing it into a register.

\ *P A definition of *\fo{VARIABLE} might be as follows:
\ *E : VARIABLE
\ **   CREATE 0 ,
\ ** ;CODE
\ **   sub     rbp, 4
\ **   mov     0 [rbp], rbx
\ **   pop     rbx
\ **   next,
\ ** END-CODE
\ **
\ ** VARIABLE TEST-VAR

\ *C CODE       \ --
\ *P A defining word used in the form:
\ *C CODE <name> ... END-CODE
\ *P Creates a dictionary entry for *\fo{<name>} to be defined by
\ ** a following sequence of assembly language words. Words defined
\ ** in this way are called *\b{code} definitions. At compile-time,
\ ** *\fo{CODE} saves the data stack pointer for later error
\ ** checking by *\fo{END-CODE}.

\ *C END-CODE   \ --
\ *P Terminates a code definition and checks the data stack pointer
\ ** against the value stored when *\fo{;CODE} or *\fo{CODE} was
\ ** executed. The assembler is disabled. See: *\fo{CODE} and
\ ** *\fo{;CODE}.

\ *C LBL:       \ --
\ *P A defining word that creates an assembler routine that can be
\ ** called from other code routines as a subroutine. Use in the form:
\ *C LBL: <name>
\ *C   ...code...
\ *C END-CODE
\ *P When *\fo{<name>} executes it returns the address of the first
\ ** byte of executable code. Later on another code definition can
\ ** call *\fo{<name>} or jump to it.

\ ****************************
\ *S Dedicated Forth registers
\ ****************************
\ *P The Forth virtual machine is held within the processor register set.
\ ** Register usage is as follows:
\ *E Forth VM registers
\ ** RBP	Data stack pointer - points to NOS
\ ** RSP        Return stack pointer
\ ** R13        Float stack pointer
\ ** RIP        Instruction pointer
\ ** RSI	User area pointer
\ ** RDI	Local variable pointer
\ **
\ ** Simulated Stack and scratch
\ ** RBX	cached top of data stack
\ ** RAX
\ ** RCX
\ ** RDX
\ ** R8..R12
\ **
\ ** Special purpose registers
\ ** R14        Index for DO/LOOP       non-volatile across ABIs
\ ** R15        Index~Limit             non-volatile across ABIs
\ ** XMM8	FTOS                    volatile Linux, non-vol Windows
\ ** XMM9       FTEMP temp float

\ *P All unused registers may be freely used by assembler routines, but they may
\ ** be altered by the operating system or wrapper calls. Before calling the
\ ** operating system, all of the Forth registers should be preserved. Before
\ ** using a register that the Forth system uses, it should be preserved and then
\ ** restored on exit from the assembler routine. Be aware, in particular, that
\ ** callbacks will generally modify the RAX register since this is used to hold
\ ** the value returned from them.

\ ***********************
\ *S Default segment size
\ ***********************
\ ** The assembler is designed to be able to generate code for
\ ** x64 CPUs in 64 bit mode with 64 bit addressing.
\ ** Any support for the old 16 or 32 bit modes is purely accidental.


\ *******************
\ *S Assembler syntax
\ *******************
\ =============================
\ *N Default assembler notation
\ =============================
\ *P The assembler is designed to be very closely compatible with MASM and other
\ ** assemblers. To this end the assembler assembles code written in the
\ ** conventional prefix notation. However, because code may be converted from
\ ** other MPE Forth systems, the postfix notation is also supported. The default
\ ** mode is prefix. The directives to switch mode are as follows:
\ *C PREFIX
\ *C POSTFIX
\ *P These switch the assembler from then onwards into the new mode. The
\ ** directives should be used outside a code definition, not within one. Their
\ ** use within a code definition will lead to unpredictable results.
\ ** MPE always uses the assembler in PREFIX mode.

\ *P The assembler syntax follows very closely that of other AMD64 assemblers.
\ ** The major difference being that the VFX Forth assembler needs white space
\ ** around everything. For example, where in MASM one might define:
\ *E MOV RAX,10[RBX]
\ *P we must write:
\ *E MOV RAX, 10 [RBX]
\ *P This distinction must be borne in mind when reading the following addressing
\ ** mode information.

\ =======================
\ *N Register to register
\ =======================
\ *P Many instructions have a register to register form. Both operands are
\ ** registers. Such an instruction is of the form:
\ *E   MOV RAX , RBX
\ *P This moves the contents of RBX into RAX. For compatibility with older MPE
\ ** assemblers the first operand may be merged with the comma thus:
\ *C   MOV RAX, RBX

\ *P This use of a register name with a 'built-in' comma also applies to other
\ ** addressing modes.

\ =================
\ *N Immediate mode
\ =================
\ *P The assembler is set for immediate-as-default. Immediate
\ ** data can also be defined explicitly (recommended). This is
\ ** done by the use of a hash (#) character:
\ *C MOV RAX, # 23
\ *P This example places the number 23 in RAX. .

\ *P The rules for instruction format and range of literals that
\ ** can be assigned to registers are arcane. The assembler does
\ ** its best to generate the shortest opcode.

\ ==============
\ *N Direct mode
\ ==============
\ *P This example places the contents of address 23 in RAX.
\ ** Direct addresses have to be specifically defined, using the *\fo{PTR} or *\fo{[]} directives:
\ *E   variable foobar
\ **   ...
\ **   MOV RAX, PTR 23
\ **   MOV RAX , [] 23
\ **   mov rcx, [] foobar
\ *P Both the above code fragments also place the contents of address 23 in RAX.

\ ======================
\ *N Base + displacement
\ ======================
\ *P Intel define an addressing mode using a base and a displacement. In this
\ ** mode, the effective address is calculated by adding the displacement to the
\ ** contents of the base register. An example:
\ *C   MOV RBX , # foobar
\ *C   MOV RAX , 10 [RBX]
\ *P In this example, RAX is filled with the contents of address foobar+10.

\ *P The assembler lays down different modes for displacements of 8-bit or 32-bit
\ ** size, but this is internal to the assembler.
\ ** The following registers may be used as base registers with a displacement:

\ *C [RAX] [RCX] [RDX] [RBX] [RBP] [RSI] [RDI]
\ *C [R8] [R9] [R10] [R11] [R12] [R13] [R14] [R15]

\ *P If the displacement is zero then the assembler internally defines the mode
\ ** as Base only. However, the displacement of zero must be supplied to the
\ ** assembler:
\ *C MOV RBX , # 0100
\ *C MOV RAX , 0 [RBX]
\ *P This places in RAX the contents of address 100 (pointed to by RBX).

\ *P The following registers may be used as a base with no displacement:
\ *C [RAX] [RCX] [RDX] [RBX] [RSI] [RDI]
\ *C [R8] [R9] [R10] [R11] [R12] [R13] [R14] [R15]

\ ==============================
\ *N Base + index + displacement
\ ==============================
\ *P The 80386 also allows two registers to be used to indirectly address memory.
\ ** These are known as the base and the index. Such instructions are of the
\ ** form:
\ *C MOV RAX , # 100
\ *C MOV RBX , # 200
\ *C MOV RDX , 10 [RAX] [RBX]

\ *P This will place in RDX the contents of address 100+200+10, or address 310.
\ ** RAX is the base and RBX is the index. Again, the displacement may be 8-bits,
\ ** 32-bits or have a value of zero. The assembler distinguishes between these
\ ** three cases. The base and index registers may be any of the following:
\ *C [RAX] [RBX] [RCX] [RDX] [RSI] [RDI]
\ *C [R8] [R9] [R10] [R11] [R14] [R15]
\ *P In addition, [RBP] may be used as the index register, and [RSP] may be used
\ ** as the base register.

\ ====================================
\ *N Base + index*scale + displacement
\ ====================================
\ *P The 80386 further supports an addressing mode where the index register is
\ ** automatically scaled by a fixed amount - either 2, 4 or 8. This is designed
\ ** for indexing into two-dimensional arrays of elements of size greater than
\ ** byte-size. One register may be used as the first index, another for the
\ ** second index, and the word size becomes implicit in the instruction. The
\ ** form of this addressing mode is very similar to that outlined above, with
\ ** the exception that the index operand includes the number which is the scale:
\ *C   MOV RBX , # 100
\ *C   MOV RCX , # 2
\ *C   MOV RAX , 10 [RBX] [RCX*4]
\ *P This stores into RAX, the contents of address 100+(4*2)+10, or address 118.
\ ** The list of registers which may be used as base is the same as the above.
\ ** The list of scaled indexes is as follows:
\ *C   [RAX*2] [RCX*2] [RDX*2] [RBX*2] [RBP*2] [RSI*2] [RDI*2]
\ *C   [RAX*4] [RCX*4] [RDX*4] [RBX*4] [RBP*4] [RSI*4] [RDI*4]
\ *C   [RAX*8] [RCX*8] [RDX*8] [RBX*8] [RBP*8] [RSI*8] [RDI*8]
\ *C   [R8*2] [RCX*2] [RDX*2] [RBX*2] [RBP*2] [RSI*2] [RDI*2]
\ *C   [R8*4] [RCX*4] [RDX*4] [RBX*4] [RBP*4] [RSI*4] [RDI*4]
\ *C   [R8*8] [RCX*8] [RDX*8] [RBX*8] [RBP*8] [RSI*8] [RDI*8]

\ ====================
\ *N Segment overrides
\ ====================
\ *P Some instructions may be prefixed with a segment override. These force data
\ ** addresses to refer to a segment other than the data segment. The override
\ ** must precede the instruction to which it relates:
\ *C   MOV RBX , # 100
\ *C   ES: MOV RAX , 10 [RBX]
\ *P This will set RAX to the value contained in address 110 in the extra
\ ** segment. The list of segment overrides is:
\ *C   FS: GS:

\ ======================
\ *N Data size overrides
\ ======================
\ *P The default data sizes are
\ ** are the default data sizes the assembler will use. If the data
\ ** is of a different size a data size override will have to be used.
\ ** To define the size of the data the following size specifiers
\ ** are used:
\ *C   BYTE or B.
\ *C   WORD or W.
\ *C   DWORD or D.
\ *C   QWORD
\ *C   TBYTE
\ *C   FLOAT
\ *C   DOUBLE
\ *C   EXTENDED

\ *P It is only necessary to specify size when ambiguity would
\ ** otherwise arise. For example:
\ *E    MOV   0 [RDX], # 10	\ can't tell
\ **    MOV   0 [RDX], RAX	\ RAX specifies 64 bit
\ *P The *\fo{BYTE} size defines that a byte operation is required:
\ *C   MOVZX RAX , BYTE 10 [RBX]

\ *P The abbreviation *\fo{B.} may also be used in place of
\ ** *\fo{BYTE} to define a byte operation. The *\fo{WORD}
\ ** specifier defines that 16-bits are required:
\ *C   MOV AX , WORD 10 [RBX]

\ *P The abbreviation *\fo{W.} may also be used to define a word
\ ** operation. *\fo{DWORD} is the default for a *\fo{USE32}
\ ** segment, and indicates that 32-bit data is to be used:
\ *C MOV RAX , DWORD 10 [RBX]
\ *C FSTP DWORD 10 [RBX]
\ *P The abbreviation *\fo{D.} may also be used to specify a
\ ** *\fo{DWORD} operation. The remaining size specifiers define
\ ** data sizes for the floating point unit.

\ *P *\fo{QWORD} defines a 64-bit operation:
\ *C FSTP QWORD 10 [RBX]
\ *P TBYTE defines a 10-byte (80-bit) operation, such as:
\ *C FSTP TBYTE 10 [RBX]

\ *P *\fo{FLOAT}, *\fo{DOUBLE} and *\fo{EXTENDED} are synonyms for
\ ** *\fo{DWORD}, *\fo{QWORD} and *\fo{TBYTE} respectively.

\ *P The segment type defines the default data size and address size for the code
\ ** in the segment. If needed, it is possible to force the data size or the
\ ** address size laid down to be the other. There is a set of data and address
\ ** size overrides which work for one instruction only. These are:
\ *C   D16:
\ *C   D16: MOV RAX , # 23

\ *P In a *\fo{USE32} or *\fo{USE64} segment, this would lay down
\ ** 16-bit data to be loaded into AX. *\fo{D16:} is almost never
\ ** needed for 64 bit programming.

\ ===============================
\ *N Near and far, long and short
\ ==============================
\ *P The default for a JMP or a CALL is within the current code
\ ** segment, whilst the default for a conditional branch is a short
\ ** branch with a -128..+127 byte range. The directives supporting
\ ** short/long and near/far are:
\ *C   SHORT  LONG

\ *P These would be used as follows:
\ *E 2 CONSTANT THAT         \ the segment number
\ ** LBL: THIS               \ the address
\ **
\ ** CALL THIS
\ ** JMP THIS
\ **
\ ** JCC THIS
\ ** JCC SHORT THIS
\ ** JCC LONG THIS

\ *P For compatibility with older MPE assemblers the mnemonics
\ ** *\fo{CALL/F}, *\fo{RET/F} and *\fo{JMP/F} are also provided.

\ ====================
\ *N Syntax exceptions
\ ====================
\ *P The assembler in VFX Forth follows both the syntax and the mnemonics defined
\ ** in the Intel Programmers Reference books.
\ ** However, there are certain exceptions. These are listed below.

\ *P The zero operand forms of certain stack register instructions for the 80387
\ ** have been omitted. Their functionality is supported however. Such
\ ** instructions are listed below, with a form of the syntax which will support
\ ** the function:
\ *E FADD    FADDP ST(1) , ST
\ ** FCOM    FCOM ST(1)
\ ** FCOMP   FCOMP ST(1)
\ ** FDIV    FDIVP ST(1) , ST
\ ** FDIVR   FDIVRP ST(1) , ST
\ ** FMUL    FMULP ST(1) , ST
\ ** FSUB    FSUBP ST(1) , ST
\ ** FSUBR   FSUBRP ST(1) , ST

\ ** Certain 80386 instructions have either one operand or two operands, of which
\ ** only one is variable. These instructions are:
\ *C MUL DIV IDIV NEG NOT
\ *P These instructions take only one operand in the VFX Forth assembler.

\ ===============
\ *N Local labels
\ ===============
\ *P If you need to use labels within a code definition, you may use the local
\ ** labels provided. These are used just like labels in a normal assembler, but
\ ** some restrictions are applied.

\ *P Ten labels are pre-defined, and their names are fixed. Additional
\ ** labels can be defined up to a maximum of 32. There is a limit of 128 forward
\ ** references.

\ ** A reference to a label is valid until the next occurrence of
\ ** *\fo{LBL:}, *\fo{CODE} or *\fo{;CODE}, whereupon all the
\ ** labels are reset.

\ *P A reference to a label in a definition must be satisfied in that definition.
\ ** You cannot define a label in one code definition and refer to it from
\ ** another.

\ *P The local labels have the names *\fo{L$1 L$2 ... L$10} and
\ ** these names should be used when referring to them e.g.
\ *C   JNE L$5

\ *P A local label is defined by words of the same names, but with a colon as a
\ ** suffix:
\ *C   L$1: L$2: ... L$10:

\ *P Additional labels (up to a maximum of 32 altogether) may be referred to by:
\ *C   n L$
\ *P where n is in the range 11..32 (decimal), and they may be defined by:
\ *C n L$:
\ *P where n is again in the range 11..32 (decimal).

\ ================
\ *N CPU selection
\ ================
\ *P This assembler is designed to cope with CPUs from 80386
\ ** upwards. Some instructions are only available on later CPUs.
\ ** Note that CPU selection affects the assembler and the VFX code
\ ** code generator, *\b{not} the run time of your application. If
\ ** you select a higher CPU level than the application runs on,
\ ** incorrect operation will occur.
\ *C  CPU=x64   \ -- ; select base AMD64 instruction set


\ ****************
\ *S Code examples
\ ****************
\ *P The best place to look for code examples is in the source
\ ** code. The file *\i{Kernel/x64Com/reqdcode.fth} contains the
\ ** code definitions required by the VFX64 kernel.

\ *E code Nrev     \ XN..X1 count -- x1..XN
\ ** \ *G Reverse the order of the top N data stack items.
\ **   cmp     rbx, # 1              \ ignore count <1
\ **   g, if,
\ **     dec     rbx                 \ item n at offset n-1
\ **     mov     rcx, rbp            \ data stack pointer
\ **     shl     rbx, # 3            \ in cells
\ **     add     rbx, rcx            \ RBX points to XN, RCX to X1
\ **     begin,
\ **       mov     rdx, 0 [rbx]      \ perform exchange
\ **       mov     rax, 0 [rcx]
\ **       mov     0 [rcx], rdx
\ **       add     rcx, # cell
\ **       mov     0 [rbx], rax
\ **       sub     rbx, # cell
\ **       cmp     rcx, rbx
\ **     a, until,
\ **   endif,
\ **   mov     rbx, 0 [rbp]
\ **   add     rbp, # cell
\ **   next,
\ ** end-code


REQUIRE END-CASE ~mak\case.f

\- >= : >= < 0= ;


MODULE: asm-access \ GASM64_MOD


\ ***********
\ Error codes (new)
\ ***********

: asm_InvAddrMode	ABORT" ASM: Invalid addressing mode" ;
: asm_invLabelRef	ABORT" ASM: invalid local label reference" ;

\ *************
\ CPU selection
\ *************

\ *********
\ Variables
\ *********

variable seg-type
  2 seg-type !			\ force it to USE64; 0=16 bit, 1 = 32 bit, 2 = 64 bit

variable d-override             \ have specified a datasize override
variable a-override             \ have specified an addresssize override

variable far/near               \ far or near call
variable short/long             \ short or long branch

variable r1                     \ number of first register
variable r2                     \ number of second register
variable r3                     \ number of third register
variable mode1                  \ mode for operand one
variable mode2                  \ mode for operand two
variable mode3                  \ mode for operand three
variable b/w/l/x1		\ size of register operand one
variable b/w/l/x2		\ size of register operand two
variable b/w/l/x3		\ size of register operand three
variable (b/w/l/x1)		\ size of register operand one, before forcing
variable mw/l/x1                \ size of memory pointer one
variable mw/l/x2                \ size of memory pointer two

variable adr-mod                \ which addressing mode to use
variable segreg                 \ which segement override is in use
variable sib-val                \ value of sib byte
variable sib-indexreg		\ sib index register number
variable sib-flag               \ says we have a sib for this instruction
variable sib-bz			\ no SIB base register
variable rex-val		\ value for REX byte
variable rex-slot		\ address of REX byte
variable rex-flag		\ true if instruction needs REX byte
variable rip-dest		\ RIP target address
variable rip-slot		\ address of 32 bit RIP offset
variable rip-flag		\ RIP needed, no forward reference
variable ripfwd-flag		\ RIP needed, has forward reference
variable 1-flag                 \ have we had a register for op1
variable 2-flag                 \ have we had a register for op2
variable #ops                   \ how many registers have we used
variable base-count             \ how many bases/indexes
variable special-reg            \ where a special register is used, op1 or op2
variable special-type           \ what type of special reg
variable memoryformat           \ datasize for '387 instructions
variable last-fwait             \ address of last FWAIT instruction
variable got-[]                 \ user has defined   [] nnn  as operand
variable co1                    \ user has specified an op1 with a ,
variable co2                    \ user has specified an op2 with a ,
variable ret-imm-flag           \ in case we have a ret # n instruction
variable dir64-flag		\ set for DIR64 moffset addressing


\ ======================
\ Segment and data sizes
\ ======================

hex

: short?                \ n -- t/f  ; true if in range -128..+127
  $80 + $100 U<
;

: ?short         \ n -- ; error if n not in -128..+127
  short? 0= ABORT" ASM: bad 8 bit signed offset" ;

: medium?               \ n -- t/f  ; true if in range -32768..+32767
  $8000 + $10000 U<
;

: ?medium                \ n -- ; error if n not in -32768..+32767
  medium? 0= ABORT" ASM: bad 16 bit signed offset" ;

: use64?                \ -- t/f ; is this a use64 segment?
  seg-type @ 2 =
;

: use32?                \ -- t/f ; is this a use32 segment?
  seg-type @ 1 =
;

: data(1)64?       \ -- t/f ; is data size operand 1 32b?
  b/w/l/x1 @ 3 =  (b/w/l/x1) @ 3 = or
;

: data64?       \ -- t/f ; is the data size 32b?
  b/w/l/x1 @ 3 =  (b/w/l/x1) @ 3 = or
  #ops @ 2 >=
  if  b/w/l/x2 @ 3 = or  then
;

: nodata64	\ -- ; error if 64 bit data
  data64? ABORT" ASM: 64 bit data invalid"
;

: data32?       \ -- t/f ; is the data size 32b?
  b/w/l/x1 @ 2 =   (b/w/l/x1) @ 2 = or
  #ops @ 2 >=
  if  b/w/l/x2 @ 2 = or  then
;

: data16?       \ -- t/f ; is the data size 16b?
  b/w/l/x1 @ 1 =  (b/w/l/x1) @ 1 = or
  #ops @ 2 >=
  if  b/w/l/x2 @ 1 = or  then
;

\ Avoid duplicates and colon defs in ASM-ACCESS

: (byte)	\ --
  b/w/l/x1 @ (b/w/l/x1) !	\ preserve the present size
  0 b/w/l/x1 !                  \ set both byte/word flags
  0 b/w/l/x2 !
  0 b/w/l/x3 !
  0 memoryformat !              \ set this (unneccessary, but complete)
;

: (word)	\ --
  b/w/l/x1 @ (b/w/l/x1) !       \ preserve the present size
  1 b/w/l/x1 !
  1 b/w/l/x2 !                  \ set both
  1 b/w/l/x3 !                  \ set both
  1 memoryformat !              \ specifies m16 for 387 (int)
;

: (dword)	\ --
  b/w/l/x1 @ (b/w/l/x1) !	\ preserve the present size
  2 b/w/l/x1 !                  \ force type of operands
  2 b/w/l/x2 !
  2 b/w/l/x3 !
  2 memoryformat !              \ specifies m32 for 387 (int)
;

: (qword)       \ --
  b/w/l/x1 @ (b/w/l/x1) !	\ preserve the present size
  3 b/w/l/x1 !                  \ force type of operands
  3 b/w/l/x2 !
  3 b/w/l/x3 !
  3 memoryformat !              \ specifies m64 for 387 (int)
;

: (tbyte)       \ --
  4 memoryformat !              \ specifies m80 for 387 (bcd)
;

: (float)       \ --
  2 memoryformat !              \ specifies m32 for 387 (real)
;

: (double)      \ --
  3 memoryformat !              \ specifies m64 for 387 (real)
;

: (extended)    \ --
  4 memoryformat !              \ specifies m80 for 387 (real)
;

\ ======================
\ Reference calculations
\ ======================

: rel8          \ addr/n --
  HERE 1+ -                  \ offset from here to addr
  dup ?short                    \ test for 8b relative
  C,                        \ stash addr here
;

: rel16         \ addr --
  HERE 2+ -                 \ offset from here to addr
  dup ?medium                   \ test for 16b relative
  W,                        \ lay it down
;

: rel32         \ addr --
  HERE 4+ - L,
;

: rel16/32              \ addr --
  use32? use64? or a-override @  xor	\ if 32/64 bit operation ; SFP064
  if  rel32  else  rel16  THEN
;

: absolute-data         \ 32b -- ; stash data
  use32? use64? or  d-override @  xor	\ 32 bit if USE32/no override
  if  L,  else  W,  THEN		\ or 16 bit/override
;

: absolute-addr         \ 32b -- ; stash data
\  use32? use64? or  a-override @  xor	\ 32 bit if USE32/no override
\  if  L,  else  W,  THEN		\ or 16 bit/override
  a-override @ 0=			\ 32 bit if no override
  if  L,  else  W,  THEN		\ or 16 bit/override;
;

: rip32,		\ 32b -- ; lay RIP offset
  rip-dest !  HERE rip-slot !	\ dest and slot addresses
  rip-flag on  0 L,			\ dummy data
;

: rip32fwd,		\ 32b -- ; lay RIP offset ; SFP066
  drop  HERE rip-slot !		\ dest and slot addresses
  ripfwd-flag on  0 L,		\ dummy data
;

: rip32         \ addr/ref --
  rip32,
;

: ?resolve-rip	\ -- ; resolve RIP slot
  rip-flag @ if				\ regular ref
    rip-dest @ HERE - rip-slot @ L!
  THEN
  ripfwd-flag @ if			\ forward RIP ref ; SFP066
    rip-slot @ HERE -		\ distance from end of ins to RIP slot
\ cr ." set [slot] @ "  rip-slot @ .dword ."  = " dup .dword
    rip-slot @ L!			\ is contents of slot
  THEN
;

: res-rip32	\ destaddr slotaddr --
\ cr ." dest = " over .dword  ." , slot = " dup .dword
\    ." , [slot] = " dup L@ .dword
  dup L@ $-010 U> ABORT" ASM: invalid RIP+offset forward reference"
  dup >r  dup L@ negate +		\ offset = dest-endOfIns ; -- dest endOfins ; R: -- slotaddr
  - r> L!
;


\ ===================================
\ Forward references and local labels
\ ===================================

128 constant maxref      \ -- u ; no. of forward references

0
  cell field fw.lab#		\ label number
  cell field fw.labaddr		\ address of label
CONSTANT /fwdref

create fwdrefs	\ -- addr ; forward reference array
  maxref /fwdref * allot
  fwdrefs maxref /fwdref * erase

\ fwdrefs structure:
\ ____ ____ ____ ____
\     |    |    |    |
\ l#  |addr|l#  |addr|     :: l# == label number, addr == address of label reference
\ ____|____|____|____|
\ 0    8    16             :: label#+`fwd' (see defs below)


32 constant maxlab      \ -- u ; no. of local labels, one cell per.
\ indexed by label number

create labtab	\ -- addr ; local label array
  maxlab cells allot
  labtab maxlab cells erase

\ labtab structure:
\ ____ ____ ____ ____
\     |    |    |    |
\ addr|addr|addr|addr|     :: addr == address of label definition
\ ____|____|____|____|
\ 0    8    16             :: label#+`label' (see defs below)

: fwd           \ label# -- addr ; `fwd'
  /fwdref * fwdrefs + ;

: label         \ label# -- addr ; `label'
  cells labtab + ;

hex

: unres-refs?   \ -- t/f ; true if unres
  0  maxref 0                           \ for the number allowed
  do
    i fwd @                             \ get the reference
    if  drop -1 leave  THEN            \ if unresolved, set true flag
  loop
;

: initfwd	\ -- ; initialize tables
  fwdrefs maxref /fwdref * erase               \ clear out both tables
  labtab  maxlab cells erase
;
\ initfwd

: (offset)      \ -- 3/5 ;
  use32? if                     \ if 32bit segment
    5                           \ need 5 to calculate 32b offset
  else                          \ if 16bit segment
    3                           \ need 3 to calculate 16b offset
  THEN
;

: ?!(t)		\ 16/32b addr -- ;
  use32? if                     \ if 32b segment
    L!                       \ stash 32bits
  else                          \ if 16bit segment
    W!                      \ stash 16bits
  THEN
;

hex

: resolve       \ n -- n ; resolve forward references
  maxref  0                             \ for each forward reference
  do
    i fwd dup fw.lab# @                 \ get the label# of the unres
    2 pick =				\ if it is this one
    if
      dup fw.labaddr @ dup C@	\ get byte at addr of label reference
      0e9 over = swap 0e8 = OR        \ jmp/call opcodes, rel16/32
      if                                \ if a 16/32bit branch
        HERE over (offset) + -      \ calculate 16 bit offset
        swap 1+ ?!(t)                   \ store rel branch in target
      else                              \ if an 8bit branch
        HERE over 2+ -              \ calculate 8 bit offset
        dup ?short                      \ range check
        swap 1+ C!                  \ store branch in target
      THEN
      0 swap !                          \ remove from table
    else				\ if it is not this one
      drop                              \ then tidy up
    THEN
  loop
;

hex

: addfwd        \ label# -- label# ; add forward reference to table
  maxref 1+ 0                           \ for quantity of legal labels
  do
    maxref i = asm_invLabelRef		\ if its too big, then error
    i fwd  dup fw.lab# @ 0=		\ if there is a zero here
    if
      2dup fw.lab# !                    \ then stash label number
      HERE swap fw.labaddr !  leave	\ stash here in next cell
    else
      drop                              \ tidy up on failure
    THEN
  loop
;

\- a-reset defer a-reset	\ -- ; reset assembler

variable <prefix>               \ prefix/postfix mode

: prefix	\ -- ; select prefix notation
  <prefix> on  ;
prefix
: postfix	\ -- ; select postfix notation
  <prefix> off  ;
: prefix?	\ -- t/f ; true if in prefix mode
  <prefix> @  ;

create aprior  0 , 0 ,          \ see ?prefix

: (?PREFIX) 	\ struct -- struct' ; executes previous opcode
\ This word assumes that the return address is on the top of the
\ return stack and that an xt is inline. It exits the caller.
  r> @					\ -- struct xt
  prefix? if
    aprior 2@  2swap  aprior 2!         \ exchange with contents of APRIOR
  THEN
  execute				\ use previous xt
;

: ?prefix  POSTPONE (?PREFIX)  HERE CELL+ , ; IMMEDIATE

: null-op       \ -- ; force execution of previous opcode
  0 ?prefix drop  a-reset  ;

: kickoff       \ n -- ; dummy operator
  drop  a-reset  ;

: set-null      \ -- ; initialise APRIOR
  0  ['] kickoff aprior 2!  ;

: !labtab       \ n -- ; set up addr of local label
  >r                            \ get label number out of way of last opcode
  null-op                       \ force to lay down last opcode - updates HERE
  r>                            \ restore label number for the resolver
  resolve                       \ resolve any that can be
  HERE swap label !         \ and stash this address in the label#
;

: @labtab       \ n -- addr ;
  dup label @                   \ get the address of this label
  swap over 0=                  \ if it is still zero
  if
    addfwd 2drop  HERE      \ then add it, and return this addr for code
  else
    drop
  THEN
;

\ get local label addr, or if undefined
\ add current dict.position to forward refs


\ local label definition

: l$:           \ label# -- ;
  dup  maxlab 1-                       \ is it
  U> asm_invLabelRef		\ within the limits
  !labtab                               \ if so, then handle it
;

\ local label reference

: l$            \ label# -- ;
  dup maxlab 1-
  U> asm_invLabelRef
  @labtab                               \ handle only if in range
;

: loc-def       \ label# -- ; -- ;
  create
    ,
  does>
    @  l$:
;

: loc-ref       \ label# -- ; -- addr ;
  create
    ,
  does>
    @  l$
;

 1 loc-def L$1:          1 loc-ref L$1
 2 loc-def L$2:          2 loc-ref L$2
 3 loc-def L$3:          3 loc-ref L$3
 4 loc-def L$4:          4 loc-ref L$4
 5 loc-def L$5:          5 loc-ref L$5
 6 loc-def L$6:          6 loc-ref L$6
 7 loc-def L$7:          7 loc-ref L$7
 8 loc-def L$8:          8 loc-ref L$8
 9 loc-def L$9:          9 loc-ref L$9
10 loc-def L$10:        10 loc-ref L$10
11 loc-def L$11:        11 loc-ref L$11


\ ====================
\ Register definitions
\ ====================

hex
\- ? : ? @ . ; 
: .info         \ -- ; used for debug only
  cr ." Sizes: " b/w/l/x1 ?  b/w/l/x2 ?  b/w/l/x3 ?
  cr ." (b/w)  " (b/w/l/x1) ?
  cr ." Width: " mw/l/x1 ? mw/l/x2 ?
  cr ." Modes: " mode1 ? mode2 ?
  cr ." r #'s: " r1 ? r2 ?
  cr ." #ops : " #ops ?
  cr
;

: ?hiReg	\ reg# -- ; ask for REX if reg# > 7
  7 > if  rex-flag on  then  ;

\- CELLS+ : CELLS+ CELLS + ;

: reg                   \ comma n size mode -- ; -- ; defining a register
  create
    , , , ,                             \ stash aspects of register
  does>
    #ops @
    case
      0 of  dup @ mode1 !               \ set mode
            dup cell+ @ b/w/l/x1 !      \ set byte/word
            dup 3 cells+ @ co1 !        \ set whether has comma or not
            2 cells+ @ dup r1 ! ?hiReg	\ set register number
            1-flag on                   \ we now have a base for reg 1
            1 #ops !                    \ and to prove it
        endof
      1 of  dup @ mode2 !               \ set mode
            dup cell+ @ b/w/l/x2 !      \ set byte/word
            dup 3 cells+ @ co2 !        \ set whether has comma or not
            2 cells+ @ dup r2 ! ?hiReg	\ set register number
            2-flag on                   \ we now have a base for reg 2
            2 #ops !                    \ count says so
        endof
      2 of  dup @ mode3 !               \ set mode
            dup cell+ @ b/w/l/x3 !      \ set byte/word
            2 cells+ @ dup r3 ! ?hiReg	\ set register number
            3 #ops !                    \ count says so
        endof
          cr #ops @ .
          cr 1 abort" #ops failure in REG"
    endcase
    base-count off                      \ we have now had a reg, and no base or index
    ret-imm-flag on                     \ in case we have a ret # n following
;

: spec-reg	\ type reg# -- ; -- ; defining a special register
  create
    , ,                                 \ save the relevant bit patterns
  does>
    #ops @                              \ what reg to define next
    case
      0 of  dup @ dup r1 ! ?hiReg	\ set the first operand
            cell+ @ special-type !      \ set the type of special reg
            1 special-reg !             \ say we have a special case in pos1
            1 #ops !                    \ increment to next reg
        endof
      1 of  dup @ dup r2 ! ?hiReg	\ must be setting the second operand
            cell+ @ special-type !      \ set the type of special reg
            2 special-reg !             \ say we have special in pos2
            2 #ops !                    \ increment to (next) reg
        endof
    endcase
;

: direct-mode   \ -- 5/6 ; direct mode number according to seg size
  use32? use64? or
  if   5                        \ set register as 5 for direct 32bit
  else 6                        \ set register number: 6 for direct 16 bit
  THEN
;

: set-direct2   \ -- ; set direct mode for op2
  direct-mode r2 !
  0 mode2 !                     \ set mode
  0 mode3 !
;

: set-direct1   \ -- ; set direct mode for op1
  direct-mode r1 !
  0 MODE1 !                     \ set mode
;

: <<3		\ n -- n<<3 ;
  3 lshift
;

: <<6		\ n -- n<<6 ;
  6 lshift
;

: [z]		\ offset struct -- offset struct ; indicates no base register ; SFP012
  #ops @ 0= if
    0 mode1 !  5 r1 !  3 mw/l/x1 !
    1-flag on
  else
    0 mode2 !  5 r2 !  3 mw/l/x2 !
    2-flag on
  THEN
  1 base-count !  sib-bz on  sib-flag on
;

: sib           \ index scale -- ; offset -- offset
  create
    , ,                         	\ lay scale then index-reg
  does>		\ offset struct --
\    sib-flag @                 	\ if there is already a sib
\    if 033 error THEN         	\ then raise an error
    base-count @ 0=			\ if no base register, force null base register ; SFP012
    if  [z]  THEN			\ SFP012
    dup @ <<6                   	\ -- ss000000 ; scale<<6
    swap cell+ @
    dup sib-indexreg !  dup ?hireg  7 and  <<3 or	\ -- ssiii000
    sib-val c!                  	\ stash it
    sib-flag on                 	\ say so
;

: ?rexByte	\ -- ; error if no REX byte
  rex-slot @ 0= if
\+ .rs  .rs
    true abort" REX byte not laid yet"
  then
;

: +rexBit	\ mask -- ; add bitmask to REX byte
  ?rexByte  rex-slot @ C@  or  rex-slot @ C!
;
: -rexBit	\ mask -- ; clear bitmask in REX byte
  rex-slot @ if
    invert  rex-slot @ C@  and  rex-slot @ C!
  else
    drop
  then
;

: +REX.B	( -- ; set REX.B )  $01 +rexBit  ;
: +REX.X	( -- ; set REX.X )  $02 +rexBit  ;
: +REX.R	( -- ; set REX.R )  $04 +rexBit  ;
: +REX.W	( -- ; set REX.W )  $08 +rexBit  ;
: -REX.W	( -- ; clr REX.W )  $08 -rexBit  ;

: rex.w,	\ -- ; lay $048 REX.W byte
  HERE rex-slot !  $048 C,
;

: rex,		\ -- ; lay $040 REX byte
  HERE rex-slot !  $040 C,
;

: ?rex,		\ -- ; lay REX if registers demand it
  rex-flag @ if  rex-slot @ 0= if  rex,  THEN  THEN
;

: ?rex64,	\ -- ; lay REX if registers or data64 demands it.
  rex-flag @ data64? or if
    rex-slot @ 0= if  rex,  THEN
    data64? if  +REX.W  THEN
  THEN
;

: ?rex(1)64,	\ -- ; lay REX if registers or operand1 size demands it.
  rex-flag @ data(1)64? or if
    rex-slot @ 0= if  rex,  THEN
    data(1)64? if  +REX.W  THEN
  THEN
;

: ?RexReg	\ reg# -- ; force REX if reg# > 7
  7 > if  rex-flag on  ?rex64,  then
;

decimal

: direct1?      \ -- t/f ; is op1 direct?
  r1 @ 7 and 5 = 			\ if direct 32/64 bit
;

: direct2?      \ -- t/f ; is op2 direct?
  r2 @ 7 and 5 = 			\ if direct 32/64 bit or
;

: (index)       \ reg-addr struct -- ; set an [index*1] register for op1/2
  @ dup sib-indexreg !  dup ?hireg  7 and <<3
  swap @ 7 and				\ ignore high part of register number
  or sib-val !                  	\ stash in buffer
  sib-flag on                   	\ say so
;

: off>mod	\ offset -- mod
\ Given an offset, return the offset size value in the mod field
\ of the mod/rm byte.
  dup 0= if  exit  then			\ return 0
  short? if  1  else  2  then
;

: notNoOffset	\ mod reg -- mod'
\ If  reg is 5 (RBP) or R13, a zero size offset not permitted because
\ mod=0, reg=5 is a 32 bit displacement (See 32 bit mod/rm addressing).
  @ 7 and 5 = 1 and ( 0/1 ) max		\ minimum of 1
;

: (base1)       \ offset addr -- offset ; set a [base] register for op1
  @ dup ?HiReg r1 !			\ set register number
  dup off>mod  r1 notNoOffset  mode1 !	\ set mode
;

: (base2)       \ offset addr -- offset ; set a [base] reg for op2
  @ dup ?HiReg r2 !		\ set register number
  dup off>mod  r2 notNoOffset  mode2 !	\ set mode
;

decimal

: (ind)         \ struct --
\ set state to indicate we have had (another) index
  dup >r                                \ stash struct
  dup cell+ @                           \ get size from struct
  #ops @ 0= if                          \ if no registers yet ...
    mw/l/x1 !				\ -- struct ; set A size
    base-count @ 0= if                  \ if no base yet
      (base1)                           \ then this is the base
      1 base-count !
      1-flag on
    else
      r1 swap (index)                   \ else this is the index*1
      base-count off
    THEN
  else
    mw/l/x2 !				\ -- struct ; set A size
    base-count @ 0= if
      (base2)                           \ either the base
      2 #ops !
      2-flag on
      1 base-count !
    else
      r2 swap (index)                   \ or the index*1
    THEN
  THEN
  r> 2 cells + @ if                     \ ends in a comma?
    #ops @
    case
      0 of  co1 on  1 #ops +!  endof	\ SFPxxx
      2 of  co2 on  ( 1 #ops +! )  endof \ SFPxxx
    endcase
  THEN
;

: ind           \ comma size n -- ;  n1 -- n1
  create
    , , ,
  does>
   (ind)
;

: reset         \ -- ;
\  .show-code

  0 r1 !  5 mode1 !		\ reset reg no as well, default immediate
  0 r2 !  5 mode2 !		\ reset reg number as well, default immediate
  0 r3 !  5 mode3 !		\ set immediate as default

  0 adr-mod !

  3 mw/l/x1 !  3 mw/l/x2 !		\ default address size

  0 (b/w/l/x1) !			\ default data size
  0 b/w/l/x1 !                  	\ set both byte/word flags
  0 b/w/l/x2 !
  0 b/w/l/x3 !
  0 memoryformat !              	\ set this (unneccessary, but complete)

  a-override off  d-override off        \ turn off overrides
  sib-flag off  sib-indexreg off        \ no sib yet
  sib-bz off
  rex-flag off  rex-slot off  rex-val off  \ no REX required
  rip-dest off  rip-slot off  rip-flag off  ripfwd-flag off  \ no RIP required
  #ops off                              \ no operands yet
  1-flag off  2-flag off                \ no base found yet on either side
  base-count off                        \ as above
  far/near off
  short/long off
  special-reg off                       \ reset this also
  got-[] off                            \ we have not had one of these yet
  dir64-flag off			\ SFP066
  co1 off  co2 off
\  log-line                              \ generate list file
\+ save-pos  save-pos                              \ save position for prefix use
  ret-imm-flag off                      \ in case we dont have a # with ret
;

' reset TO a-reset

: dir1?         \ -- t/f ; is register1 direct?
  direct1?
  mode1 @ 0= and			\ and mode 0
  sib-flag @ 0= and			\ SFP012
;

: dir2?         \ -- t/f ; is register2 direct?
  direct2?
  mode2 @ 0= and			\ and mode 0
  sib-flag @ 0= and			\ SFP012
;

\ Modes
\ 0..3		as mod field in r/m, 0=0[reg], 1=8b[reg], 2=32b[reg], 3=reg
\ 4		segment register
\ 5		immediate
\ 6		PC
\ 7		segment/offset

\ 9		ST(x), NDP FP register

\ 11/0B		XMMx register

: areg?          \ addr -- t/f ;
  @ 3 =
;

: seg?          \ addr -- t/f ;
  @ 4 =
;

: xreg?          \ addr -- t/f ;
  @ $B =
;

: #?            \ -- t/f ;
\ true if second operand is immediate
  mode2 @
 5 =
;

: #3?           \ -- t/f ;
\ true if third operand is immediate
  mode3 @ 5 =
;

: ac?           \ addr -- t/f ;
  @ 0=
;

: word?         \ -- t/f ; used in XCHG only
  b/w/l/x1 @
  b/w/l/x2 @
  and			\ are both operands word ones?
;

hex

: i/d?          \ addr -- t/f ;
  @ 2 U<=         \ indirect or direct ?
;

: r/i/d?        \ addr -- t/f ;
  @ 3 U<=        \ register, indirect or direct?
;

: must-be       \ n -- ;
  0= if
\    .rs
    asm_InvAddrMode		\ error 21 if false
  then
;

: must-be-reg1	\ --
  mode1 areg? must-be
;

: must-be-reg2	\ --
  mode2 areg? must-be
;

: must-be-mem1       \ -- ; is op.1 indirect or direct,
  mode1 i/d? must-be                  \ error if not
;

: must-be-mem2       \ -- ; is op.2 indirect or direct,
  mode2 i/d? must-be                  \ error if not
;

: must-be-r/m1	\ --
  mode1 r/i/d? must-be                  \ error if not
;

: must-be-r/m2	\ --
  mode2 r/i/d? must-be                  \ error if not
;

[UNDEFINED] either= [IF]
: either=       \ n a b -- t/f ; true if n=a or n=b
  2 pick = -rot                 \ n, b
  =  or                         \ n, a
;
[THEN]
: r1=16/64	\ -- ; error if not 16 or 64 bit data
  b/w/l/x1 @ 1 3 either= must-be
;

: r1=32/64	\ -- ; error if not 32 or 64 bit data
  b/w/l/x1 @ 2 3 either= must-be
;

: r1=16/32/64	\ -- ; error if not 16, 32 or 64 bit data
  b/w/l/x1 @  1- 2 U<= (  1 3 within?) must-be
;

: r2=32/64	\ -- ; error if not 32 or 64 bit data
  b/w/l/x2 @ 2 3 either= must-be
;

: 0op1          \ n -- ; -- ; single byte 0 operand instructions
  create
    ,                           \ lay down byte
  does>
    ?prefix                     \ do the last one
    @ C,                    \ comma it
    reset                       \ reset for next instruction
;

: 0op1-64       \ n -- ; -- ; single byte 0 operand instructions
  create  ,  does> ?prefix
    rex.w,			\ REX.W
    @ C,                    \ comma it
    reset                       \ reset for next instruction
;

: 0op1-32       \ n -- ; -- ; single byte 0 operand instructions
  create
    ,                           \ lay down byte
  does>
    ?prefix                     \ do the last one
    @ C,                    \ comma it
    reset                       \ reset for next instruction
;

: 0op1-16	\ n -- ; -- ; single byte 0 operand instructions
  create
    ,                           \ lay down byte
  does>
    ?prefix                     \ do the last one
    $66 C,			\ then lay down 16 bit override
    @ C,                    \ comma it
    reset                       \ reset for next instruction
;

: 0op2		\ n1 n2 -- ; two byte 0-op instructions
  create
    , ,                         \ lay down bytes
  does>
    ?prefix                     \ do the last one
    dup cell + @ C,		\ comma first
    @ C,                    \ comma second
    reset                       \ reset for next instruction
;

hex

: ax/al,#       \ n opcode -- ; puts down opcode and data for ax or al
  b/w/l/x1 @ if
    1+ C, absolute-data     \ 1/2/3: word, laid down accordingly
  else
    C, C,               \ 0:byte
  THEN
;

: +gpr1		\ opc -- opc' ; add GPR in bits 2:0 ; see push/pop, xchg
  r1 @ 7 and  or
  r1 @ 7 > if  +REX.B  then
;

: +gpr2		\ opc -- opc' ; add GPR in bits 2:0 ; see xchg
  r2 @ 7 and  or
  r2 @ 7 > if  +REX.B  then
;

: +mod+r/m1     \ n1 -- n2 ; adds mod and r/m for register 1
  sib-flag @ 0= if			\ all done if we already have a SIB
    mode1 @ 3 <> if			\ not a register, so x [Rx]
      r1 @ 7 and 4 = if			\ reg is RSP or R12, so special
        sib-flag on			\ need a SIB
        r1 @ 7 > if  +REX.B  then
        $04 r1 !			\ set R1=4 to force SIB & no index
        $20 sib-val !			\ no index indicator (ss=0, index=4)
      then
    then
  then
  mode1 @ <<6 +  r1 @ 7 and +
  r1 @ 7 > if  +REX.B  then
;

: +mod+r/m2     \ n1 -- n2 ; adds mod and r/m for register 2
  sib-flag @ 0= if			\ all done if we already have a SIB
    mode2 @ 3 <> if			\ not a register, so x [Rx]
      r2 @ 7 and 4 = if			\ reg is RSP or R12, so special
        sib-flag on			\ need a SIB
        r2 @ 7 > if  +REX.B  then
        $04 r2 !			\ set R1=4 to force SIB & no index
        $20 sib-val !			\ no index indicator (ss=0, index=4)
      then
    then
  then
  mode2 @ <<6 +  r2 @ 7 and +
  r2 @ 7 > if  +REX.B  then
;

: +reg1         \ n1 -- n2 ; adds reg for register 1
  r1 @ 7 and <<3 +
  r1 @ 7 > if  +REX.R  then
;

: +reg2         \ n1 -- n2 ; adds reg for register 2
  r2 @ 7 and  <<3 +
  r2 @ 7 > if  +REX.R  then
;

: opcode+w1     \ opcode -- opcode'
  b/w/l/x1 @
  if  1+  THEN
;

: opcode+w2     \ opcode -- opcode'
  b/w/l/x2 @
  if  1+  THEN
;

: opcode+sw,    \ imm-op opcode -- imm-op
  opcode+w1
  b/w/l/x1 @ if
    over short?
    if  2+  THEN
  THEN
  C,
;

: disp,         \ displacement reg mode --
  sib-bz @ if
    2drop  absolute-addr  exit
  then
  case					\ for MOD/RM and normal SIB
    0  of
         dup 5 = swap $D = or		\ RIP at reg=5/13
         sib-flag @ 0= and		\ and NO SIB byte
         if  rip32  else  drop  THEN
       endof
    1  of  drop  C,  endof
    2  of  drop  absolute-addr   endof
           asm_InvAddrMode
  end-case
;

hex

: disp1,        \ disp -- ;
  r1 @  mode1 @  disp,
;

: disp2,        \ disp -- ;
  r2 @  mode2 @  disp,
;

: ?disp1,       \ disp -- ;
  mode1 areg? 0=
  if  disp1,  THEN
;

: ?disp2,       \ disp -- ;
  mode2 areg? 0=
  if  disp2,  THEN
;

hex

: ?REX.X	\ -- ; check for REX.X
  sib-indexreg @ 7 > if  +REX.X  then
;

: lay-sib1      \ -- ;
  sib-flag @ if                 \ if a sib byte is required...
    sib-val c@                  \ get the ssiii000 part
    r1 @ $07 and  or C,     \ merge base register
    r1 @ 7 > if  +REX.B  then
    ?REX.X
  THEN
;

: lay-sib2      \ -- ;
  sib-flag @ if                 \ if a sib byte is required...
    sib-val c@                  \ get the ssiii000 part
    r2 @ $07 and  or C,     \ merge base register
    r2 @ 7 > if  +REX.B  then
    ?REX.X
  THEN
;

: ?sib,         \ 8b sib# -- ; maybe lay a sib
  swap  sib-flag @ if                   \ if sib required
    $0F8 and  $04 or  C,		\ mask sib bit, lay r/m
    1 =                                 \ depending whether sib1 or sib2
    if   lay-sib1                       \ also lay sib1
    else lay-sib2                       \ or sib2
    THEN
  else                                  \ if no sib required
    C,                              \ lay straight
    drop                                \ ignore the sib number
  THEN
;

: ?ext1,        \ disp 8b -- ; maybe lay sib and displacemnt
\ cr ." 8b = "  dup .byte  ."  SIBF = " sib-flag @ .  mode1 @ .  r1 @ .
  1 ?sib,                               \ maybe lay a sib
  ?disp1,                               \ maybe lay a displacement
;

: ?ext2,        \ disp 8b -- ; maybe lay sib and displacemnt
  2 ?sib,                               \ maybe lay a sib
  ?disp2,                               \ maybe lay a displacement
;

: m/r,m/r?      \ -- t/f
  mode1 areg? if
    mode2 r/i/d?
  else
    mode2 areg? if
      mode1 r/i/d?
    else
      0
    THEN
  THEN
;

: mem/reg,#     \ [ displacement ] imm-op op-code adr-mod --
  >r opcode+sw, r>                      \ -- (disp) io a-m
  +mod+r/m1                             \ -- (disp) io a-m'
  1 ?sib,
  >r ?disp1, r>
  b/w/l/x1 @ if
    dup short?
    if    C,
    else  absolute-data
    THEN
  else
    C,
  THEN
;

: mem/reg,#l	\ [ displacement ] imm-op opcode adr-mod --
  swap
  opcode+w1 C,
  +mod+r/m1 1 ?sib,
  >r ?disp1, r>
  b/w/l/x1 @                              \ lay down the immediate data
  if
    absolute-data
  else
    C,
  THEN
;

: mem/reg,mem/reg       \ [ offset ] op-code --
  mode1 areg?                            \ if reg1 is r/m
  if
    2+ opcode+w1 C,                 \ make opcode
    0 +mod+r/m2
    +reg1                               \ make basic modr/m
    ?ext2,
  else                                  \ if not...
    mode2 areg? must-be                  \ it must be reg2 is reg
    opcode+w2 C,                    \ lay opcode
    0 +mod+r/m1
    +reg2                               \ make basic modr/m
    ?ext1,
  THEN
;

hex

: lay-addr-override     \ -- ; lay the addr override
  $067 C,                   \ lay addr override
  a-override on                 \ say override set
;

: lay-data-override     \ -- ; lay the data override
  $066 C,                   \ lay it
  d-override on                 \ say override set
;

: ?lay-manual-override  \ -- t/f ; is a manual override requested?
  0                             \ dummy false flag
  d-override @                  \ if data override spec'd
  if
    lay-data-override           \ lay it
    drop -1                     \ say override laid
  THEN
  a-override @                  \ if address override spec'd
  if
    lay-addr-override           \ lay it
    drop -1                     \ say override laid
  THEN
;

: 2*data32?     \ -- t/f ; are both data sized 32b?
  b/w/l/x1 @ 2 =
  b/w/l/x2 @ 2 = and
;

: 2*data16?     \ -- t/f ; are both data sized 16b?
  b/w/l/x1 @ 1 =
  b/w/l/x2 @ 1 = and
;

: addr16?       \ -- t/f ; is the address size 16b?
  mw/l/x1 @ 1 =
  mw/l/x2 @ 1 = or
;

: addr32?       \ -- t/f ; is the address size 32/64b?
  addr16? 0=
;

: ?both         \ -- ; maybe lay both overrides
\  data32? use16? and  addr32? and
\  data16? use32? and  addr16? and  or
  data16? addr16? and if
    lay-addr-override
    lay-data-override
  THEN
;

: ?data-override        \ -- ; maybe lay data override
\  data32? use16? and addr16? and	\ 0
\  data16? use32? and addr32? and or
\  2*data16? use32? and or
\  2*data32? use16? and or
  data16? 2*data16? or
  if  lay-data-override  THEN
;

: ?addr-override        \ -- ; maybe lay an address override
\  data32? use32? and addr16? and
\  data16? use16? and addr32? and or
  addr16?
  if  lay-addr-override  THEN
;

: ?lay-auto-override    \ -- ; maybe lay an override automatically
  ?addr-override
  ?data-override
  ?both
;

: ?overrides	\ -- ; maybe lay an override
  ?lay-manual-override 0=               \ if no manual override needed
  if  ?lay-auto-override  THEN         \ then maybe lay one anyway
;

: ?in-override  \ -- ; maybe lay a data override for IN
\  b/w/l/x1 @ 1 =  use32? and    	\ if IN AX 32b seg
\  b/w/l/x1 @ 2 =  use16? and or	\ or IN EAX 16b seg
  b/w/l/x1 @ 1 =
  if  lay-data-override  THEN
;

: ?out-override \ -- ; maybe lay a data override for IN
  b/w/l/x2 @ 1 =
  if  lay-data-override  THEN
;

: ?auto-movx-overrides          \ -- ; see if an auto one needed for movxx
  (b/w/l/x1) @ 1 =  b/w/l/x1 @ 1 = or
  mw/l/x2 @ 0=  b/w/l/x2 @ 0= or   and
  if  lay-data-override  THEN		\ then lay an override
;

: ?movx-overrides       \ -- ; maybe lay an override for MOVxX
  ?lay-manual-override 0=               \ if no manual override needed
  if  ?auto-movx-overrides  THEN       \ then see if an auto one
;

: 3modes	\ o1 o2 o3 am -- ; [ displacement ] [ imm-op ] --
  create
    , , , ,
  does>
    ?prefix                             \ do the last one
    ?overrides                          \ lay any data or address override
    ?rex64,				\ see if REX byte needed
    dup @ adr-mod !  #? if
      mode1 areg?  r1 ac? and if
        2 cells+ @ ax/al,#              \ lay immediate,ax form
      else
        cell+ @  adr-mod @  mem/reg,#   \ lay immediate,reg form
      THEN
    else
      m/r,m/r? must-be                  \ must be r/m
      3 cells+ @
      mem/reg,mem/reg                   \ then do it
    THEN
    ?resolve-rip reset
;

: ac/reg,#	\ imm-op ^op -- ; SFP012
\ Select EAX,#n if r1=EAX and imm-op cannot be a byte value, or
\ reg,#n if it can (shorter opcode).
  mode1 areg?  r1 ac? and if		\ ac,#n?
    over short? 0= if			\ if not byte, ACC form is shorter
\    over short? 0=  use16? or if	\ if not byte, ACC form is shorter
      2 cells + @ ax/al,#  exit		\ ACC,immediate form
    THEN
  THEN
  cell+ @  adr-mod @  mem/reg,#		\ immediate, long/short form ; SFP002
;

: 3modes/l      \ o1 o2 o3 am -- ; [ displacement ] [ imm-op ] --
  create
    , , , ,
  does>
    ?prefix                     	\ do the last one
    ?overrides
    ?rex64,				\ see if REX byte needed
    dup @ adr-mod !  #? if
      ac/reg,#
    else
      m/r,m/r? must-be
      3 cells + @ mem/reg,mem/reg	\ mem/reg form
    THEN
    ?resolve-rip reset
;

: ?[]           \ -- ;
  got-[] @
  if  set-direct2  THEN
;

: ?[]1          \ -- ;
  got-[] @
  if  set-direct1  THEN
;

: (jmp)         \ addr -- ;
  ?overrides
  ?rex64,
  dir1? got-[] @ 0= and		\ if op1 is direct; ie JMP xxxx
  mode1 @ 5 = or		\ or if op1 is immediate
\  intel? mode1 @ 5 = and or \ or if op1 is immediate and mode is intel
  if
    dup HERE 2+ - short?    \ if short because of size,
    short/long @ 0= and       \ and LONG not selected
    if
      $0EB C,               \ JMP REL8 opcode
      rel8                      \ and relative 8bit address
    else                        \ if long
      $0E9 C,               \ JMP REL16/32 opcode
      rel16/32                  \ and relative 16/32 bit address
    THEN
  else                          \ JMP RM16/32; if register specified
    ?[]1                        \ this mode
    mode1  r/i/d? must-be       \ must be these choices
    $0ff C,                 \ magic number
    $020 +mod+r/m1              \ basic modr/m
    ?ext1, ?resolve-rip
  THEN
  reset
;

: (jmp/f)               \ seg addr -- ;
  ?overrides
  dir1? got-[] @ 0= and		\ if no regs or [], then must be direct
  mode1 @ 5 =  or		\ or if op1 is immediate
\  intel? mode1 @ 5 = and or	\ or if op1 is immediate and mode is intel
  if
    $0EA C,                 \ JMP PTR far opcode
    absolute-addr  W,        \ 32/16b offs 16b seg
  else                          \ if indirect, ie JMP [] xxxx
    ?[]1 must-be-mem1           \ must be these modes
    $0FF C,                 \ JMP MEM far opcode
    $028 +mod+r/m1              \ build modr/m
    ?ext1, ?resolve-rip
  THEN
  reset
;

: (call)                \ addr -- ;
  ?overrides
  ?rex64,
  dir1? got-[] @ 0= and		\ if no regs or [], then must be direct
  mode1 @ 5 =  or		\ or if op1 is immediate and mode is intel
\  intel? mode1 @ 5 = and or	\ or if op1 is immediate and mode is intel
  if
    $0E8 C,                 \ CALL REL16/32 opcode
    rel16/32                    \ lay relative address
  else                          \ if a reg or a [], then  must be indirect
    ?[]1                        \
    mode1 r/i/d? must-be        \ must be register/immediate/direct
    $0ff C,                 \ CALL RM16/32 opcode
    $010 +mod+r/m1              \ make modr/m
    ?ext1, ?resolve-rip
  THEN
  reset                         \  finished the opcode
;

: (call/f)      \ seg off -- ;
  ?overrides
  dir1? got-[] @ 0= and		\ if no regs or [], then must be direct
  mode1 @ 5 =  or		\ or if op1 is immediate and mode is intel
\  intel? mode1 @ 5 = and or	\ or if op1 is immediate and mode is intel
  if
    $09A C,                 \ CALL FAR ss oooo
    absolute-addr W,         \ lay offset then segment - 16/32o:16s
  else                          \ with [], then must be indirect
    ?[]1 must-be-mem1
    $0ff C,                 \ CALL FAR [] oooo
    $018 +mod+r/m1
    ?ext1, ?resolve-rip
  THEN
  reset
;

: bit-scan      \ n1 n2 -- ; (reg mem) -- ;
  create
    , ,
  does>
    ?prefix                     \ do the last one
    ?overrides
    ?rex64,			\ see if REX byte needed
    dup cell+ @ C,          \ lay first byte
    @ C,                    \ lay second byte
    mode1 areg? must-be         \ the only way to be
    0 +mod+r/m2
    +reg1                       \ generate modr/m
    ?ext2,
    ?resolve-rip reset
;

: bit-test      \ reg b1 b2a b2b /r -- ; (reg/mem reg/imm) -- ;
  create
    >r  , , , ,  r> ,		\ last is /R for R/M for immediate
  does>
    ?prefix                     \ do the last one
    ?overrides
    ?rex64,			\ see if REX byte needed
    dup 2 cells+ @ C,       \ lay first byte
    mode2 @ 5 <> if		\ if not immediate
      cell+ @ C,            \ lay second byte for reg form
      0 +mod+r/m1               \
      +reg2                     \ generate modr/m
      ?ext1,
    else
      dup @ C,              \ lay second byte for imm form
      4 cells + @ 3 lshift	\ /R value
      +mod+r/m1			\ plus mod/rm
\      0 +mod+r/m1               \ generate base modr/m
\      swap 0c + @ <<3 or      \ add /4 bit from databook
      1 ?sib,
      mode1 areg? 0=
      if  swap disp1,  THEN	\ lay down any displacement or imm
      C,                    \ lay immediate data
    THEN
    ?resolve-rip reset
;

: 2op-r,r/m     \ op1 op2 -- ; reg16,32 reg/mem16,32 -- ;
  create
    , ,
  does>
    ?prefix                     \ do the last one
    mode1 areg? must-be		\ r1 must be a register
    ?overrides                  \ overrides do apply
    ?rex64,			\ see if REX byte needed
    dup cell+ @  C,
    @  C,                   \ lay two bytes of opcode
    0 +mod+r/m2 +reg1           \ basic modr/m
    ?ext2,
    ?resolve-rip reset
;

: 0F,2op-r/m,r/m	\ op2 op3 -- ; e.g. MOVBE
  create  swap ,  ,  does> ?prefix >r
    r1=16/32/64			\ size 16/32/64
    ?overrides                  \ overrides do apply, e.g. 66
    ?rex64,			\ see if REX byte needed
    $0F C,			\ 0F prefix
    r@ @ C,			\ 38 1st opcode
    r> cell+ @
    mode2 areg? 1 and dup >r
    or C,			\ F0/F1 2nd opcode
    r> if
      0 +mod+r/m1 +reg2  ?ext1,
    else
      0 +mod+r/m2 +reg1  ?ext2,
    then
    ?resolve-rip reset
;

: F3,2op-r,r/m     \ op1 op2 -- ; reg16,32,64 reg/mem16,32 -- ; LZCNT
  create  , ,  does> ?prefix	\ do the last one
    ?overrides                  \ overrides do apply, e.g. 66
    $F3 C,			\ F3 prefix
    ?rex64,			\ see if REX byte needed
    mode1 areg? must-be		\ r1 must be a register
    r1=16/32/64			\ size 16/32/64
    dup cell+ @  C,
    @  C,                   \ lay two bytes of opcode
    0 +mod+r/m2 +reg1           \ basic modr/m
    ?ext2,
    ?resolve-rip reset
;

: 2op-r/m,r	\ opc -- ; CMPXCHG  r/m, r ; SFP016
  create
    ,
  does> ?prefix
    ?overrides				\ overrides do apply
    ?rex64,				\ see if REX byte needed
\ cr mode1 @ .  r1 @ .  mode2 @ .  r2 @ .  #ops @ .
    mode2 areg? must-be			\ it must be mode2 is reg
    $0F C,				\ prefix opcode
    @ opcode+w2 C,			\ lay primary opcode
    0 +mod+r/m1  +reg2  ?ext1,		\ make basic modr/m
    ?resolve-rip  reset
;

: 1op-r32		\ opc -- ; BSWAP and friends ; SFP016
  create
    ,
  does> ?prefix
    mode1 areg? must-be			\ it must be mode is reg
    b/w/l/x1 @ 2 = must-be		\ size must be 32 bits
    ?overrides				\ overrides do apply
    $0F C,				\ prefix opcode
    @ r1 @ OR C,			\ lay primary opcode
    reset
;

: 1op2-m8	\ opc1 opc2 r/m -- ; CLFLUSH
  create  rot ,  swap ,  ,  does> ?prefix >r
    ?overrides
    ?rex64,
    b/w/l/x1 @ 0= must-be		\ size must be 8 bits bits
    mode1 r/i/d? must-be		\ no segs, or #
    r@ @ C,  			\ opc1
    r@ cell+ @ C,			\ opc2
    r> 2 cells + @ +mod+r/m1 ?ext1, 	\ opc3+r/m
    ?resolve-rip reset          	\ as ever
;

: 1op3		\ opc1 opc2 opc3 -- ; LFENCE
  create  rot ,  swap ,  ,  does> ?prefix >r
    r@ @ C,  			\ opc1
    r@ cell+ @ C,			\ opc2
    r> 2 cells + @ C,	 	\ opc3
    reset
;

: load-table    \ op1 op2 reg -- ; mem -- ;
  create
    , , ,
  does>
    ?prefix                     \ do the last one
    mode1 areg? 0= must-be      \ must be a mem operand
    ?rex64,				\ see if REX byte needed
    dup 2 cells+ @ C,       \ lay first byte
    dup cell+ @ C,          \ lay second byte
    0 +mod+r/m1                 \ modr/m bits of that byte
    swap @                      \ get reg bit of modr/m
    <<3 +                       \ add reg bit
    ?ext1,
    ?resolve-rip reset          \ as ever
;

: 2op-r/m       \ op1 op2 reg -- ; reg/mem16 -- ;
  create
    , , ,
  does>
    ?prefix                     \ do the last one
    ?rex64,				\ see if REX byte needed
    b/w/l/x1 @ 1 = must-be      \ must be 16b register or effective addr
    dup 2 cells+ @ C,       \ lay first opcode byte
    dup cell+ @ C,          \ lay second byte of opcode
    0 +mod+r/m1                 \ basic modr/m
    swap @ <<3 +                \ add ref bit to it
    ?ext1,
    ?resolve-rip reset
;

: mov(x)x       \ op1 op16 op32 -- ; reg reg/mem -- ;
  create
    , , ,
  does>
    ?prefix                             \ do the last one
    ?movx-overrides                     \ special case overrides
    ?rex64,				\ see if REX byte needed
    dup 2 cells+ @ C,               \ always lay first byte of instruction
    b/w/l/x2 @ 0= mw/l/x2 @ 0= or       \ if source is byte
    if    cell+ @ C,                \ lay first version
    else  @ C,                      \ lay second version
    THEN
    0 +mod+r/m2                         \ get basic modr/m
    +reg1                               \ make further modr/m
    ?ext2,
    ?resolve-rip reset                  \ finished
;

: 0op16/32      \ size op -- ; -- ;
  create
    , ,
  does>
    ?prefix                             \ do the last one
    dup                                 \ extra copy of address of data
    cell+ @ 1 =                         \ if 16b instruction
    if  lay-data-override  THEN	\ lay override
    @ C,                            \ get instruction and lay it
    reset
;

: set           \ op2 -- ; r/m8 -- ; SETcc
  create
    ,
  does>
    ?prefix                     \ do the last one
    ?rex64,			\ see if REX byte needed
    0f C,                   \ lay first byte
    @ C,                    \ lay second byte of opcode
    0 +mod+r/m1
    ?ext1,
    ?resolve-rip reset          \ reset for next instruction
;

: d-shift       \ op1 op2 -- ;
  create
    , ,
  does>
    ?prefix                     \ do the last one
    ?overrides                  \ in case its the wrong size
    ?rex64,				\ see if REX byte needed
    dup cell+ @ C,          \ lay first byte of opcode
    @                           \ get second byte of opcode
    mode3 @ 5 <>                \ if op3 is not immediate
    if                          \ if op3 is register
      1+                        \ add to shift to non-immediate form
    THEN
    C,                      \ lay second byte of opcode
    0 +mod+r/m1 +reg2           \ make basic modr/m
    1 ?sib,                     \ lay it and any sib byte
    mode1 areg? 0=               \ if there is a displacement to lay
    if
      mode3 @ 5 =               \ if there is also immediate data
      if swap THEN             \ then swap to get the displacement
      disp1,                    \ then lay it
    THEN
    mode3 @ 5 =                 \ if op3 is immediate data
    if  C,  THEN           \ lay it down
    ?resolve-rip reset          \ and now we have finished
;

: imul(0)       \ [disp] -- ; IMUL R/M form
  0f6 opcode+w1 C,          \ opcode, w for 16/32/64
  028 +mod+r/m1                 \ make mod byte
  ?ext1,                        \ lay all necessary bytes
;

: imul(1a)      \ imm# -- ; IMUL R,I
  dup short?
  if                            \ if reg, imm8
    06b C,                  \ lay byte of opcode
    0 +mod+r/m1 +reg1           \ generate basic modr/m
    C,                      \ lay it
    C,                      \ lay immediate data
  else                          \ if reg, imm16/32
    069 C,                  \ lay byte of opcode
    0 +mod+r/m1 +reg1           \ generate basic modr/m
    C,                      \ lay it
    b/w/l/x1 @ 1 =                \ is it 16 bit target register
    if    W,                 \ lay immediate data (16b)
    else  L,                 \ lay immediate data (32b)
    THEN
  THEN
;

: imul(1b)      \ [disp] -- ; IMUL R,R/M
  0f C,  0af C,         \ lay bytes of opcode
  0 +mod+r/m2 +reg1             \ make basic modr/m
  ?ext2,
;

: imul(1)       \ [disp] -- ; IMUL R,R/M/I forms
  #?                            \ have immediate?
  if                            \ mode2=imm; IMUL R,I
    imul(1a)
  else                          \ mode2=mem; IMUL R,R/M
    imul(1b)
  THEN
;

: imul(2)       \ [disp] -- ; the IMUL R,R/M,I form
  dup >r                        \ immediate data out the way
  short?                        \ is immediate data 8b or not
  if                            \ if 8 bit data
    06b C,                  \ lay opcode
    0 +mod+r/m2 +reg1           \ basic modr/m
    ?ext2,
    r> C,                   \ lay 8b immediate data
  else                          \ if 16 or 32 bit data
    069 C,                  \ lay opcode
    0 +mod+r/m2 +reg1           \ basic modr/m
    ?ext2,
    r>                          \ get immediate data back
    b/w/l/x1 @ 1 =              \ if reg1 is 16bit
    if    W,                 \ .. lay 16b immediate
    else  L,                 \ else lay 32b immediate
    THEN
  THEN
;

: inc/dec       \ o2 am --
  create
    , ,				\ am o2
  does>
    ?prefix                     \ do the last one
    ?overrides
    ?rex64,			\ see if REX byte needed
    dup @ adr-mod !
    cell+ @			\ o2
    b/w/l/x1 @ 1 min
    or C,
    adr-mod @
    +mod+r/m1
    ?ext1, ?resolve-rip
    reset
;

: mul/div       \ o1 am --
  create
    ,
  does>
    ?prefix                     \ do the last one
    ?overrides
    ?rex64,			\ see if REX byte needed
    @ adr-mod !
    $0F6 opcode+w1 C,       \ magic number
    adr-mod @ +mod+r/m1
    ?ext1, ?resolve-rip
    reset
;

: j/lp          \ o16 -- ; addr -- ;
  create
    ,
  does>
    ?prefix                     \ do the last one
    short/long @ 0=             \ if default/short
    if
      @ C, rel8             \ lay byte and rel branch
    else                        \ if long
      $0f C,                \ lay first byte
      @ $010 + C,           \ lay second byte
      rel16/32                  \ lay rel branch as per seg size
    THEN
    reset
;

: (reg,mem)	\ [reg] [mem] -- ; SFP011
  mode1 areg? must-be		\ must be register
  mode2 r/i/d? must-be		\ no segs, or #
  0 +mod+r/m2 +reg1
  ?ext2,
;

: reg,mem       \ o1 -- ; reg mem -- ;
  create
    ,
  does>
    ?prefix                     \ do the last one
    ?overrides                  \ any overrides needed
    ?rex64,			\ see if REX byte needed
    @ C,                    \ comma in opcode
    (reg,mem)			\ lay rest ; SFP011
    ?resolve-rip reset
;

: reg,mem(2)            \ o1 o2 -- ; reg mem -- ;
  create
    , ,
  does>
    ?prefix                     \ do the last one
    ?overrides                  \ any overrides needed
    ?rex64,			\ see if REX byte needed
    dup cell+ @ C,          \ comma in first opcode
    @ C,                    \ comma in second opcode
    (reg,mem)			\ lay rest ; SFP011
    ?resolve-rip reset
;

: reg,mem(3)	\ o1 o2 -- ; reg mem -- ; SFP011
  create
    , ,
  does>
    ?prefix                     \ do the last one
    ?overrides                  \ any overrides needed
    ?rex64,			\ see if REX byte needed
    dup cell+ @ C,          \ comma in first opcode
    @ C,                    \ comma in second opcode
    b/w/l/x1 @ must-be		\ no byte operations
    (reg,mem)			\ lay rest
    ?resolve-rip reset
;

: reg,mem(sz)       \ o1 -- ; reg mem -- ; for MOVSXD
  create
    ,
  does>
    ?prefix                     \ do the last one
    ?overrides                  \ any overrides needed
    ?rex(1)64,			\ see if REX byte needed
    @ C,                    \ comma in opcode
    r1=32/64  2 b/w/l/x2 !	\ reg=32/64, r/m=32
    (reg,mem)			\ lay rest ; SFP011
    ?resolve-rip reset
;

: lay-spec      \ -- ;
  3 <<6                 	\ mod part is always `11'
  special-reg @ 1 = if		\ if special is in pos1
    r1 @ 7 and <<3 or       	\ mask in the special reg#
    r2 @ 7 and or           	\ mask in general reg#
  else                          \ if special is in pos2
    r2 @ 7 and <<3 or       	\ mask in the special reg#
    r1 @ 7 and or           	\ mask in general reg#
  THEN
  C,                	\ lay down modr/m byte
;

: mov-spec      \ -- ;
  b/w/l/x1 @ 3 = b/w/l/x2 @ 3 = or must-be        \ must be 32b reg involved
  special-reg @ 1 = if                  \ if special is operand1
    r1 @ 7 > r2 @ 7 > or if
      rex,
      r1 @ 7 > if  +REX.R  then
      r2 @ 7 > if  +REX.B  then
    then	\ REX.B
    $0f C,                          \ lay down first byte of opcode
    case  special-type @                \ is it DRn, CRn or TRn ?
      1 of  $023 C, lay-spec endof	\ type DRn, lay opcode
      2 of  $022 C, lay-spec endof  \ type DRn, lay opcode
      3 of  $026 C, lay-spec endof  \ type DRn, lay opcode
          true abort" Failure with type in mov-spec"
    endcase
  else                                  \ if special is operand2
    r1 @ 7 > r2 @ 7 > or if
      rex,
      r1 @ 7 > if  +REX.B  then
      r2 @ 7 > if  +REX.R  then
    then	\ REX.B
    case  special-type @                \ is it DRn, CRn or TRn ?
      1 of  $021 C, lay-spec  endof	\ type DRn, lay opcode
      2 of  $020 C, lay-spec  endof \ type DRn, lay opcode
      3 of  $024 C, lay-spec  endof \ type DRn, lay opcode
         true abort" Failure with type in mov-spec"
    endcase
  THEN
  reset
;

: pop/push      \ fs gs o1 am1 o2 o3 --
  create
    , , , , , ,
  does> ?prefix				\ do the last one
    mode1 @ 5 = if			\ if immediate data from the PUSH intruction
      drop                              \ drop the address of the data .. its fixed
      dup short?  b/w/l/x1 @ 0= and if	\ SFP0037
        $06A C,  C,		\ lay short form, lay immediate 8b data
      else
        ?overrides
        $068 C,  absolute-data	\ lay opcode and immediate data
      THEN
      reset  exit
    THEN
    mode1 seg? if
      r1 @ 4 < if                       \ if DS, ES or SS
        @ r1 @ <<3 or C,		\ seg reg operand
      else                              \ if FS or GS
        $0F C,                      \ lay first byte
        r1 @
        case
          4 of  5 cells+ @ C, endof \ the FS byte
          5 of  4 cells+ @ C, endof \ the GS byte
        endcase
      THEN
    else
      ?overrides
      r1=16/64				\ data is 16 or 64 bit
      mode1 areg? if			\ register operand
        r1 @ 7 > if			\ REX.B for high regs
          HERE  rex-slot !
          $041 C,
        then
        cell+ @  r1 @ 7 and +  C,	\ reg operand
      else
        ?rex64,
        mode1 r/i/d? must-be
        dup 3 cells+ @ C,
        2 cells+ @ +mod+r/m1
        ?ext1, ?resolve-rip
      THEN
    THEN
    reset
;

: shifts        \ am -- ;
  create
    ,
  does> @
    ?prefix                             \ do previous instruction
    ?overrides
    mode1 r/i/d? must-be                \ mem/reg,
    ?rex64,				\ see if REX byte needed
    #? if
      swap dup >r  1 =                  \ shift by 1/n
      if  $0D0  else  $0C0  THEN
      opcode+w1 C, +mod+r/m1 ?ext1,
      r> dup 1 <>			\ SFP001
      if  C,  else  drop  THEN	\ n>1 lay n
    else
      $0D2 opcode+w1 C,             \ cl
      +mod+r/m1 ?ext1,
    THEN
    ?resolve-rip reset
;

: rets          \ op -- ;
  mode1 @ 5 =                   \ is the operand immediate
  ret-imm-flag @ and if       \ and been defined with #
    C, W,               \ always 16b immediate data
  else
    1+ C,
  THEN
  reset                         \ reset at end
;

: returns       \ o(near) o(far) --
  create
    , ,
  does>
    ?prefix                     \ do the last one
    far/near @ 0=
    if  cell+  THEN            \ near: point to the next opcode in word
    @ rets                      \ do the stuff
;

: lay-seg       \ seg -- ; -- ; set the segment# override
  create
    ,                           \ lay segment number
  does>
    ?prefix                     \ do the last one
    @ dup C,                \ lay the segment
    segreg !                    \ set reg to say which
    reset                       \ must do this, therefore xx: must be *before* instruction
;

: segreg?	\ -- ;
  segreg @ ?dup if		\ if a segment override is used:
    -1 allot		\ remove fwait opcode
    C,  segreg off          \ lay segreg byte, and reset
  THEN
;

: (fwait)	\ -- ; lays wait opcode
  HERE 1-  last-fwait @  <> if      \ previous byte <> fwait
    HERE last-fwait !               \ mark where laid
    $09B C,                         \ lay opcode: magic number
  THEN
;

: f0op          \ op1 op2 -- ; -- ;
\ simple F.P. no operand instructions (no FWAIT): Fxxx
  create
    , ,                         \ lay down the two bytes of opcode
  does>
    ?prefix                     \ do the last one
    #ops @ 0= must-be           \ must be no operands
    dup                         \ spare copy of address
    cell+ @ C,              \ fetch first byte, lay it
    @ C,                    \ fetch second byte, lay it
    reset                       \ finished
;

: f0op(w)       \ op1 op2 -- ; -- ; simple no operand instructions
\ simple F.P. no operand instructions (with FWAIT): Fxxx
  create
    , ,                         \ lay down the two bytes of opcode
  does>
    ?prefix                     \ do the last one
    #ops @ 0= must-be           \ must be no operands
    (fwait)                     \ lay an fwait
    dup                         \ spare copy of address
    cell+ @ C,              \ fetch first byte, lay it
    @ C,                    \ fetch second byte, lay it
    reset                       \ finished
;

: fs1/0         \ op1 op2 -- ; -- ;
\ opcodes with either zero or one stack operand: Fxxx or Fxxx ST(n)
  create
    , ,                         \ lay opcodes
  does>
    ?prefix                     \ do the last one
    #ops @ 0= if		\ if no registers
      dup cell+ @ C,        \ lay first byte
      @ 1+ C,               \ lay default second byte
    else                        \ if any registers
      mode1 @ 9 = must-be       \ must be a stack register
      dup cell+ @ C,        \ lay first byte
      @ r1 @ or C,        \ lay second byte with register number
    THEN
    reset
;

: fs1           \ op1 op2 -- ; -- ;
\ simple one-operand instruction: Fxxx ST(n)
  create
    , ,                         \ lay opcodes
  does>
    ?prefix                     \ do the last one
    #ops @ 1 =                  \ must only have one operand
    mode1 @ 9 = and must-be	\ and it must be a st.
    dup cell+ @ C,		\ lay first byte
    @ r1 @ or  C,           \ get second op, add reg#, lay it
    reset                       \ finished
;

: fm16/32       \ op32 op16 mask -- ; -- ;
\ one operand (either m32 or m16): Fxxx m32 or Fxxx m16
  create
    , , ,                       \ lay the three required bytes
  does>
    ?prefix                     \ do the last one
    ?rex64,			\ see if REX byte needed
    dup
    memoryformat @ 2 = if       \ if m32
      2 cells+                  \ point to opcode
    else                        \ if m16
      cell+                     \ point to opcode
    THEN
    @ C,                    \ get opcode and lay it
    0 +mod+r/m1         	\ generate basic modr/m
    swap @                      \ get reg mask
    <<3 +                       \ generate whole modr/m
    ?ext1, ?resolve-rip
    reset
;

: fs1,s		\ op1 op2 -- ; -- ;
\ simple expression using two stack operators: Fxxx ST(n), ST
  create
    , ,                                 \ lay opcodes
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 = mode2 @ 9 = and       \ must be st(n), st(n)
    r2 @ 0= and must-be               \ and must be st(n), st0
    dup cell+ @ C,                  \ get first op, lay it
    @ r1 @ or  C,                 \ get second op, add reg#, lay it
    reset                               \ finished
;

: fmxx          \ op1 reg-mask -- ; -- ;
\ one memory operand (indeterminate size): Fxxx mxx
  create
    , ,                                 \ lay mask and opcode
  does>
    ?prefix                             \ do the last one
    ?rex64,				\ see if REX byte needed
    mode1 @ 9 <> must-be                \ must not be a stack reg.
    dup cell+ @ C,                  \ lay byte of opcode
    0 +mod+r/m1                         \ generate modr/m byte
    swap @ <<3 +                        \ modify with reg-mask bits
    ?ext1, ?resolve-rip
    reset
;

: fmxx(w)       \ op1 reg-mask -- ; -- ;
\ one memory operand (indeterminate size) (with FWAIT): Fxxx mxx
  create
    , ,                                 \ lay mask and opcode
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 <> must-be                \ must not be a stack reg.
    (fwait)                             \ lay fwait
    ?rex64,				\ see if REX byte needed
    dup cell+ @ C,                  \ lay byte of opcode
    0 +mod+r/m1                         \ generate modr/m byte
    swap @ <<3 +                        \ modify with reg-mask bits
    ?ext1, ?resolve-rip
    reset
;

: fm/ax         \ op1 op2 regmask wait? -- ; -- ;
\ either m16 or AX, with or without a FWAIT
  create
    , , , ,                             \ lay down bytes
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 <> must-be                \ must not be a stack reg.
    dup @                               \ if a wait is needed
    if  (fwait)  THEN                  \ lay it
    dup 3 cells+ @ >r                   \ get the byte
    r1 @ 0= b/w/l/x1 @ 1 = and if       \ if reg1== AX
      r> 2 + C,                     \ lay AX form of first opcode
      2 cells+ @ C,                 \ lay second byte of opcode
    else                                \ if mem16
      ?rex64,				\ see if REX byte needed
      r> C,                         \ lay mem form of first opcode
      0 +mod+r/m1                       \ generate basic modr/m
      swap cell+ @ <<3 +                \ mask in regmask bits
      ?ext1, ?resolve-rip
    THEN
\   (fwait)                             \ dont lay fwait
    reset                               \ finished
;

: m16           \ addr -- reg-mask ;
  dup 3 cells + @ C,	\ lay op1
  2 cells+ @                    \ get reg-mask1
;

: m32           \ addr -- reg-mask ;
  dup cell+ @ C,            \ lay op2
  2 cells+ @                    \ get reg-mask1
;

: m64           \ addr -- reg-mask ;
  dup 3 cells+ @ C,         \ lay op1
  @                             \ get reg-mask2
;

: fm16/32/64    \ op1 reg-mask1 op2 reg-mask2 -- ; -- ;
\ either m16, m32 or m64: Fxxx m16 or Fxxx m32 or Fxxx m64
  create
    , , , ,                     \ lay opcode and reg mask
  does>
    ?prefix                     \ do the last one
    mode1 @ 9 <> must-be        \ must not be a stack reg
    ?rex64,			\ see if REX byte needed
    memoryformat @              \ depending on mxx size
    case
      1 of  m16 endof           \ do the 16bit form
      2 of  m32 endof           \ do the 32bit form
      3 of  m64 endof           \ do the 64bit form
        abort" Failure with memoryformat in FP opcode"
    endcase
    0 +mod+r/m1 swap <<3 +      \ generate modr/m
    ?ext1, ?resolve-rip
    reset                       \ finished
;

: fm32/64/s     \ op1 op2 op3 reg-mask -- ; -- ;
\ m32, m64 or st(n)
  create
    , , , ,                     \ lay down bytes
  does>
    ?prefix                     \ do the last one
    mode1 @ 9 = if              \ if reg1 is a stack reg
      dup 2 cells+ @ C,     \ lay the first byte of this form
      cell+ @ r1 @ + C,     \ get second byte and add reg#, then lay
    else                        \ if the operand is memory
      ?rex64,			\ see if REX byte needed
      dup                       \ spare copy of address
      memoryformat @ 2 = if     \ if the mem32 form
        3 cells+                \ point to first byte of this form
      else                      \ if the mem64 form
        2 cells+                \ point to first byte of this form
      THEN
      @ C,                   \ get first byte and lay it
      0 +mod+r/m1 swap @ <<3 +  \ make modr/m byte
      ?ext1, ?resolve-rip
    THEN
    reset
;

: fm32/64/80/s  \ op1 op2 op3 op4 op5 reg-mask1 reg-mask2 -- ; -- ;
\ m32, m64, m80 or st(n)
  create
    , , , , , , ,                       \ lay down bytes
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 = if                      \ if reg1 is a stack reg
      dup 3 cells+ @ C,             \ lay the first byte of this form
      2 cells+ @ r1 @ + C,          \ get second byte and add reg#, then lay
    else                                \ if the operand is memory
      ?rex64,				\ see if REX byte needed
      memoryformat @ 1 > must-be        \ fail if wrong size
      dup                               \ spare copy of address
      case  memoryformat @
        2 of 6 cells+ @ C, cell +  endof \ mem32: lay first byte of this form
        3 of 5 cells+ @ C, cell +  endof \ mem64: lay first byte of this form
        4 of 4 cells+ @ C,         endof \ mem80: lay first byte of this form
      endcase
      0 +mod+r/m1  swap @ <<3 +		\ make modr/m byte
      ?ext1, ?resolve-rip
    THEN
    reset
;

: fm/s1/0       \ op32 mask32 op64 mask64 op1st op2st op10 op20 -- ; -- ;
\ m32, m64, st(n) or no operands
\ NB form below ignores zero-op case, user must do Fxxx ST(1)
  create
    , , , , , , , ,                     \ lay them all
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 = if                      \ if operand is stack(n)
      dup 3 cells+ @ C,              \ lay first opcode
      2 cells+ @                        \ get second opcode
      r1 @ + C,                      \ add reg# and lay opcode
    else
      memoryformat @ 1 > must-be        \ must be float or double
      ?rex64,				\ see if REX byte needed
      memoryformat @ 2 = if		\ if m32
        dup 7 cells+ @ C,           \ lay first opcode
        6 cells+ @                      \ get reg mask
      else                              \ if m64
        dup 5 cells+ @ C,           \ lay first opcode
        4 cells+ @                      \ get reg-mask
      THEN
      <<3
      0 +mod+r/m1 +                     \ make modr/m byte
      ?ext1, ?resolve-rip
    THEN
    reset                               \ finish up
;

: fm/ss/0       \ reg-mask op2 -- ; -- ;
\ m32, m64 st,st1 or st1,st
\ NB form below ignores zero-op case, user must do Fxxx ST(1)
  create
    , ,                                 \ lay down these bytes
  does>
    ?prefix                             \ do the last one
    mode1 @ 9 = if                      \ if either st1,st or st,st1
      r1 @ 0= if                        \ if st,st1
        $0D8 C,                     \ lay first byte
        @ r2 @ + C,                 \ get second byte, add r#, and lay
      else                              \ if st1,st
        $0DC C,                     \ lay first byte
        @ r1 @ + C,                 \ get second byte, add r#, and lay
      THEN
    else                                \ if m32 or m64
      ?rex64,				\ see if REX byte needed
      memoryformat @ 2 = if             \ if m32
        $0D8 C,                     \ lay first byte
      else                              \ if m64
        $0DC C,                     \ lay first byte
      THEN
      cell+ @ <<3 0 +mod+r/m1 +         \ generate modr/m
      ?ext1, ?resolve-rip
    THEN
    reset                               \ finished
;

hex

: asm-init      \ cs dp --
  segreg off
  last-fwait off                     \ reset assembler
  reset  initfwd
;                                    \ reset assembler

: asm-on	\ -- ; turn assembler on
  asm-init                              \ initialise assembler
  also asm-access
;

: asm-off       \ -- ; turn assembler off
  previous
;

: (enter)       \ imm16 imm8 -- ;
  mode1 @ 5 = mode2 @ 5 =  and must-be        \ must be imm, imm; (mode3 due to `,')
  $0C8 C,                                   \ lay opcode
  swap  W,                                  \ lay imm16
  C,                                        \ lay imm8
  reset                                         \ ready for next one
;

: (imul)        \ ??? -- ;
  ?overrides
  ?rex64,			\ see if REX byte needed
\ cr co1 @ .  co2 @ .
  co1 @ if                      \ if we have had at least op1,op2
    co2 @ if
      imul(2)                   \ the IMUL R,R/M,I form
    else                        \ if we have had op1,op2
      imul(1)                   \ the IMUL R,R/M/I form
    THEN
  else                          \ if we have had op1
    imul(0)                     \ the IMUL R/M form
  THEN
  ?resolve-rip reset
;

: (esc)         \ -- ;
  ?overrides
  ?rex64,			\ see if REX byte needed
  $0D8 C,                   \ ESC opcode
  0 +mod+r/m2 C,
  ?disp2, ?resolve-rip
  reset
;

: (in)          \ ??? -- ;
  nodata64
  ?in-override
  mode1 areg?
  r1 ac? and  must-be
  #? if
    $0E4 opcode+w1 C,  C,        \ magic number
  else
    mode2 areg?
    r2 @ 2 = and  must-be
    $0EC opcode+w1 C,                \ magic number
  THEN
  reset
;

: (out)         \ ??? -- ;
  nodata64
  ?out-override
  mode2 areg?
  r2 ac?  and must-be
  mode1 @ 5 = if			\ if immediate data
    $0E6 opcode+w2 C,  C,	\ OUT #,R
  else
    mode1 areg?                         \ OUT DX,R
    r1 @ 2 = and  must-be
    $0EE opcode+w2 C,               \ magic number
  THEN
  reset
;

: (int)         \ n --
  dup 3 = if				\ if int3, then $CC, else $CD
    drop  $0CC C,
  else
    $0CD C,  C,
  THEN
  reset
;

\ The MOV reg64, imm instruction has several forms
\   mov  rbx, # imm64             ; full 64 bit literal
\   mov  rbx, # simm32            ; sign extended 32 bit literal
\ when a 32 bit register is modified, it is zero extended to 64 bits, so
\   mov  ebx, # uimm32            ; zero extended 32 bit literal

\ FIX THESE FOR SIGNED 32 BIT IMMEDIATES
\ 64 BIT HOST ASSUMED
: immlong?	\ immval -- flag ; true if long form required
  data64? if
    $80000000 + $100000000 U<
  else
    drop true
  then
;

: (mov)         \ ??? --
  special-reg @ 0<> if                  \ if a special reg has been used
    mov-spec  exit                      \ do the special case
  THEN
  #? if                                 \ if immediate source
    mode1 areg? if
      dup immlong? if
        ?overrides
        ?rex64,				\ see if REX byte needed
        r1 @ 7 > if  +rex.b  then
        $0b0  b/w/l/x1 @ 1 min <<3 +  r1 @ 7 and +  C,    \ opcode; 8b or other
        case  b/w/l/x1 @
          0 of  C,  endof
          1 of  W,  endof
          2 of  L,  endof
          3 of  ,  endof
              asm_InvAddrMode
        endcase
        ?resolve-rip reset  exit
      THEN
    THEN
    mode1 r/i/d? must-be		\ [ displacement ] imm-op --
    ?overrides
    ?rex64,				\ see if REX byte needed
    $0C6 0 mem/reg,#l			\ magic number
    ?resolve-rip reset  exit
  THEN
  mode2 seg? if
    mode1 r/i/d? must-be
    ?rex64,				\ see if REX byte needed
    $08C C, 0 +mod+r/m1		\ magic number
    +reg2
    ?ext1,
    ?resolve-rip reset  exit
  THEN
  mode1 seg? if				\ seg. reg. for dest
    mode2 r/i/d? must-be
    ?rex64,				\ see if REX byte needed
    $08E C, 0 +mod+r/m2		\ magic number
    +reg1
    ?ext2,
    ?resolve-rip reset  exit
  THEN
  mode2 areg?
  r2 ac? and
  dir1? and				\ mem, ac
  dir64-flag @ and if			\ SFP066
    ?overrides
    ?rex64,
    $0A2 opcode+w2 C,  ,	\ magic number
    reset  exit
  THEN
  mode1 areg?
  r1 ac? and
  dir2? and				\ ac, mem
  dir64-flag @ and if			\ SFP066
    ?overrides
    ?rex64,
    $0A0 opcode+w1 C, ,		\ magic number
    reset  exit
  THEN
  m/r,m/r? if				\ mem/reg,mem/reg
    ?overrides
    ?rex64,				\ see if REX byte needed
    $088 mem/reg,mem/reg		\ magic number
    ?resolve-rip reset  exit
  THEN
  asm_InvAddrMode
;

: (test)        \ ??? -- ;
  ?overrides
  ?rex64,				\ see if REX byte needed
  #? if
    mode1 areg?
    r1 ac? and if
      $0a8 ax/al,#
    else
      mode1 r/i/d? must-be
      $0f6 0 mem/reg,#l
    THEN
  else
    mode2 areg? must-be                 \ must be register
    mode1 r/i/d? must-be                \ no segs, or #
    $084 opcode+w1 C,
    0 +mod+r/m1 +reg2
    ?ext1,
  THEN
  ?resolve-rip reset
;

: (xchg)        \ ??? -- ;
  ?overrides
  ?rex64,			\ see if REX byte needed
  r1 ac?
  mode2 areg? and
  word? and if			\ XCHG AX,R
    $090 +gpr2 C,			\ SFP071\
\    $090 r2 @ or C,
  else
    r2 ac?
    mode1 areg? and
    word? and if		\ XCHG R,AX
      $090 +gpr1 C,			\ SFP071
\      $090 r1 @ or C,
    else
      mode1 areg? if		\ XCHG R,M
        $086 opcode+w1 C,
        0 +mod+r/m2
        +reg1
        ?ext2,
      else                      \ XCHG M,R
        $086 opcode+w2 C,
        0 +mod+r/m1 +reg2
        ?ext1,
      THEN
    THEN
  THEN
  ?resolve-rip reset
;

: (,)		\ -- ; define comma separator for the assembler
  #ops @ 2 <  co1 @ 0= and if \ if: ...
    co1 on                      \ then force: ...,
    1 #ops !
    exit
  THEN
  co1 @ if                      \ if: ...,...
    co2 on                      \ then force: ...,...,
    2 #ops !
  THEN
;

: (ptr)         \ -- ; specifies direct rather than immediate
  #ops @ 0= if                  \ what reg to define next
    set-direct1                 \ set op1 to direct mode
  else
    set-direct2                 \ set op2 to direct mode
  THEN
  1 #ops +!
;

\ *****************************
\ SSE/SSE2 and XMM reg handling
\ *****************************

: lay3-sse	\ struct --
  dup w@ dup $FFFF =			\ if mandatory prefix
  if  drop  else  C,  then		\ lay it
  ?rex64,
  dup cell+ @ C,			\ first opcode
  2 cells + @ C,			\ second opcode
;

: lay2/3-sse	\ struct --
  dup w@ dup $FFFF =			\ if mandatory prefix
  if  drop  else  C,  then		\ lay it
  ?rex64,
  cell+ @ C,			\ first opcode
;

: must-be-xreg2	\ --
  mode2 xreg? must-be			\ must be XMM reg
;

: must-be-xreg1	\ --
  mode1 xreg? must-be			\ must be XMM reg
;

: must-be-xmm1	\ --
  must-be-xreg1  3 mode1 !		\ convert to ordinary reg mode
;

: must-be-xmm2	\ --
  must-be-xreg2  3 mode2 !		\ convert to ordinary reg mode
;

: must-be-xmem1	\ --
  mode1 areg? 0= must-be		\ must not be integer register
  mode1 xreg? 				\ if src = XMMn
  if  3 mode1 !  then			\ convert to ordinary reg mode
;

: must-be-xmem2	\ --
  mode2 areg? 0= must-be		\ must not be integer register
  mode2 xreg? 				\ if src = XMMn
  if  3 mode2 !  then			\ convert to ordinary reg mode
;

: (FXSAVExx)	\ -- ; [disp] --
  ?rex64,  $0F C,  $AE C,
  mode1 areg? 0= must-be		\ must be a memory address
  0 +mod+r/m1 ?ext1,
  ?resolve-rip reset
;

: (xreg,xmem)	\ [disp] --
\ dest=XMMn, src = XMMn/m128
  must-be-xmm1 must-be-xmem2	\ xmm1, xmm2/rm
  0 +mod+r/m2 +reg1 ?ext2,
;

: xreg,xmem	\ op1 op2 op3 -- ; [disp] --
\ ADDSS  xmm1, xmm2/m32/64/128
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse
  (xreg,xmem)
  ?resolve-rip reset
;

: cc-xreg,xmem	\ op1 op2 op3 imm8 -- ; [disp] --
\ CMPEQPD  xmm1, xmm2/m32/64/128
  create  >r  rot ,  swap ,  ,  r> ,  does> ?prefix
  dup lay3-sse  3 cells + @ >r		\ -- R: -- imm8
  (xreg,xmem)				\ lay R/m sib offset
  r> C,				\ lay imm8
  ?resolve-rip reset
;

: xreg,r/m32/64	\ op1 op2 op3 -- ; [disp] --
\ CVTSI2SD xmmn, r/m32  CVTSI2SD xmmn, r/m64
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse				\ lay opcodes
  must-be-xmm1  must-be-r/m2  r2=32/64	\ check operands
  0 +mod+r/m2 +reg1 ?ext2,
  ?resolve-rip reset
;

: reg32/64,xmem	\ op1 op2 op3 -- ; [disp] --
\ CVTSD2SI reg32/64, xmm/m64
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse				\ lay opcodes
  must-be-reg1 r1=32/64			\ check operand1
  must-be-xmem2				\ check operand2
  0 +mod+r/m2 +reg1 ?ext2,
  ?resolve-rip reset
;

: sz-force	\ 32/64 --
  case
    32 of  (dword)  endof
    64 of  (qword)  endof
  endcase
;

: op3-memsz	\ op1 op2 op3 /x sz -- ; [disp] --
\ FXSAVE mem32  FXSAVE64 mem64
  create  >r >r  rot ,  swap ,  ,  r> , r> ,  does> ?prefix
  dup 3 cells + @ >r			\ /x
  dup 4 cells + @ sz-force		\ data size
  lay3-sse				\ lay opcodes
  must-be-mem1				\ check operand1 must be memory
  r> 3 lshift +mod+r/m1 ?ext1,
  ?resolve-rip reset
;

: xreg,xreg	\ op1 op2 op3 -- ; --
\ MASKMOVDQU xmm1, xmm2
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse				\ lay opcodes
  must-be-xmm1 must-be-xmm2		\ check operands
  0 +mod+r/m2 +reg1 ?ext2,
  ?resolve-rip reset
;

: xmem,xmem	\ op1 op2 op3 op4 -- ; [disp] --
\ MOVAPD  xmm1, x/m  MOVAPD  x/m, xmm2
  create  >r  rot ,  swap ,  ,  r> ,  does> ?prefix
  dup lay2/3-sse			\ -- struct ; prefix and first opcode
  mode1 xreg? if			\ XMM1
    must-be-xmm1  must-be-xmem2
    2 cells + @ C,
    0 +mod+r/m2 +reg1 ?ext2,
  else
    must-be-xmem1  must-be-xmm2	\ XMM2
    3 cells + @ C,
    0 +mod+r/m1 +reg2 ?ext1,
  then
  ?resolve-rip reset
;

: gen-movsd	\ op1 op2 op3 op4 -- ; [disp] --
\ MOVSD  xmm1, x/m  MOVSD  x/m, xmm2   MOVSD  (no operands, string copy)
  create  >r  rot ,  swap ,  ,  r> ,  does> ?prefix
  #ops @ 0= if				\ will be zero for string compare
    drop  $A5 C,  reset  exit
  then
  dup lay2/3-sse			\ -- struct ; prefix and first opcode
  mode1 xreg? if			\ XMM1
    must-be-xmm1  must-be-xmem2
    2 cells + @ C,
    0 +mod+r/m2 +reg1 ?ext2,
  else
    must-be-xmem1  must-be-xmm2	\ XMM2
    3 cells + @ C,
    0 +mod+r/m1 +reg2 ?ext1,
  then
  ?resolve-rip reset
;

: xr/r/m,xr/r/m	\ op1 op2 op3 op4 32/64 -- ; [disp] --
\ MOVDX  xmm1, r/m  MOVDX  r/m, xmm2
  create  >r >r  rot ,  swap ,  ,  r> ,  r> ,  does> ?prefix
  dup 4 cells + @ sz-force		\ data size
  dup lay2/3-sse			\ -- struct ; prefix and first opcode
  mode1 xreg? if			\ XMM1
    must-be-xmm1  ( must-be-r/m2 )
    2 cells + @ C,
    0 +mod+r/m2 +reg1 ?ext2,
  else
    ( must-be-r/m1 )  must-be-xmm2 	\ XMM2
    3 cells + @ C,
    0 +mod+r/m1 +reg2 ?ext1,
  then
  ?resolve-rip reset
;

: gen-movq	\ op1 op2 op3 op4 64 -- ; [disp] --
\ Handle MOVQ all forms
  create  >r >r  rot ,  swap ,  ,  r> , r> ,  does> ?prefix
  mode1 xreg?  mode2 xreg? and if
    drop				\ discard struct
    (qword)				\ must be 64 bit
    must-be-xmm1 must-be-xmm2
    $F3 C,  ?rex64,  $0F C,  $7E C,	\ special form
    0 +mod+r/m2 +reg1 ?ext2,
    ?resolve-rip reset
    exit
  then
  dup 4 cells + @ sz-force		\ data size
  dup lay2/3-sse			\ -- struct ; prefix and first opcode
  mode1 xreg? if			\ XMM1
    2 cells + @ C,
    must-be-xmm1  must-be-r/m2
    0 +mod+r/m2 +reg1 ?ext2,
  else
    3 cells + @ C,
    must-be-r/m1  must-be-xmm2  	\ XMM2
    0 +mod+r/m1 +reg2 ?ext1,
  then
  ?resolve-rip reset
;


: reg,xreg	\ op1 op2 op3 -- ; --
\ MOVMSK reg, xreg
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse				\ lay opcodes
  must-be-reg1 must-be-xmm2		\ check operands
  0 +mod+r/m2 +reg1 ?ext2,  reset
;

: reg,xreg,imm8	\ op1 op2 op3 -- ; imm8 --
\ PEXTRW reg, xreg, # imm8
  create  rot ,  swap ,  ,  does> ?prefix \ imm8 struct --
  swap >r
  lay3-sse				\ lay opcodes
  must-be-reg1 must-be-xmm2  #3? must-be  \ check operands
  0 +mod+r/m2 +reg1 ?ext2,  		\ there's no displacement!
  r> C,  reset
;

: mem,xreg	\ op1 op2 op3 -- ; [disp] --
\ MOVNTDQ mem, xreg
  create  rot ,  swap ,  ,  does> ?prefix
  lay3-sse
  must-be-mem1  must-be-xmm2		\ check operands
  0 +mod+r/m1 +reg2 ?ext1,  reset
;

: xreg,r32/m16,imm8	\ op1 op2 op3 -- ; [disp] imm8 --
\ PINSRW xmm, r/m, # imm8
  create  rot ,  swap ,  ,  does> ?prefix  \ [disp] imm8 struct --
  swap >r				\ preserve imm8
  lay3-sse				\ lay opcodes
  must-be-xmm1  must-be-r/m2  #3? must-be  \ check operands
  0 +mod+r/m2 +reg1 ?ext2,		\ operands 1 and 2
  r> C,				\ operand 3
  ?resolve-rip reset
;

: xreg,xmem,imm8	\ op1 op2 op3 -- ; [disp] imm8 --
\ PSHUFD xmm, xmem, # imm8
  create  rot ,  swap ,  ,  does> ?prefix  \ [disp] imm8 struct --
  swap >r				\ preserve imm8
  lay3-sse				\ lay opcodes
  must-be-xmm1 must-be-xmem2  #3? must-be  \ check operands
  0 +mod+r/m2 +reg1 ?ext2,
  r> C,
  ?resolve-rip reset
;

: xreg,imm8	\ op1 op2 op3 /x -- ; imm8 --
  create  >r  rot ,  swap ,  ,  r> ,  does> ?prefix
  dup 3 cells + @ >r  lay3-sse		\ /x ; lay opcodes
  must-be-xmm1 #? must-be		\ check operands
  r> 3 lshift +mod+r/m1	?ext1,		\ r/m = xmmm only (no displacement)
  C,  reset				\ imm8
;

: xreg,xmem/imm8	\ op1 op2 op3 op4 /x -- ; [disp] [imm8] --
\ PSLLW xreg, xmem  PSLLW xreg, # imm8
  create  >r >r  rot ,  swap ,  ,  r> , r> ,  does> ?prefix
  dup lay2/3-sse  #? if			\ op1
    swap >r				\ -- struct ; R: -- imm8
    must-be-xmm1
    dup 3 cells + @ C,		\ op4
    4 cells + @ 3 lshift +mod+r/m1 ?ext1, \ r/m
    r> C,				\ imm8
  else
    must-be-xmm1  must-be-xmem2
    2 cells + @ C,			\ op3
    0 +mod+r/m2 +reg1 ?ext2,		\ r/m
  then
  ?resolve-rip reset
;

decimal
\ comma n size mode --
1  0 4 11 reg XMM0,   1  1 4 11 reg XMM1,   1  2 4 11 reg XMM2,   1  3 4 11 reg XMM3,
1  4 4 11 reg XMM4,   1  5 4 11 reg XMM5,   1  6 4 11 reg XMM6,   1  7 4 11 reg XMM7,
1  8 4 11 reg XMM8,   1  9 4 11 reg XMM9,   1 10 4 11 reg XMM10,  1 11 4 11 reg XMM11,
1 12 4 11 reg XMM12,  1 13 4 11 reg XMM13,  1 14 4 11 reg XMM14,  1 15 4 11 reg XMM15,

0  0 4 11 reg XMM0    0  1 4 11 reg XMM1    0  2 4 11 reg XMM2    0  3 4 11 reg XMM3
0  4 4 11 reg XMM4    0  5 4 11 reg XMM5    0  6 4 11 reg XMM6    0  7 4 11 reg XMM7
0  8 4 11 reg XMM8    0  9 4 11 reg XMM9    0 10 4 11 reg XMM10   0 11 4 11 reg XMM11
0 12 4 11 reg XMM12   0 13 4 11 reg XMM13   0 14 4 11 reg XMM14   0 15 4 11 reg XMM15

$66 $0F $58 xreg,xmem ADDPD
 -1 $0F $58 xreg,xmem ADDPS
$F2 $0F $58 xreg,xmem ADDSD
$F3 $0F $58 xreg,xmem ADDSS
$66 $0F $54 xreg,xmem ANDPD
 -1 $0F $54 xreg,xmem ANDPS
$66 $0F $55 xreg,xmem ANDNPD
 -1 $0F $55 xreg,xmem ANDNPS

$66 $0F $C2 0 cc-xreg,xmem CMPEQPD
$66 $0F $C2 1 cc-xreg,xmem CMPLTPD
$66 $0F $C2 2 cc-xreg,xmem CMPLEPD
$66 $0F $C2 3 cc-xreg,xmem CMPUNORDPD
$66 $0F $C2 4 cc-xreg,xmem CMPNEQPD
$66 $0F $C2 5 cc-xreg,xmem CMPNLTPD
$66 $0F $C2 6 cc-xreg,xmem CMPNLEPD
$66 $0F $C2 7 cc-xreg,xmem CMPORDPD

 -1 $0F $C2 0 cc-xreg,xmem CMPEQPS
 -1 $0F $C2 1 cc-xreg,xmem CMPLTPS
 -1 $0F $C2 2 cc-xreg,xmem CMPLEPS
 -1 $0F $C2 3 cc-xreg,xmem CMPUNORDPS
 -1 $0F $C2 4 cc-xreg,xmem CMPNEQPS
 -1 $0F $C2 5 cc-xreg,xmem CMPNLTPS
 -1 $0F $C2 6 cc-xreg,xmem CMPNLEPS
 -1 $0F $C2 7 cc-xreg,xmem CMPORDPS

$F2 $0F $C2 0 cc-xreg,xmem CMPEQSD
$F2 $0F $C2 1 cc-xreg,xmem CMPLTSD
$F2 $0F $C2 2 cc-xreg,xmem CMPLESD
$F2 $0F $C2 3 cc-xreg,xmem CMPUNORDSD
$F2 $0F $C2 4 cc-xreg,xmem CMPNEQSD
$F2 $0F $C2 5 cc-xreg,xmem CMPNLTSD
$F2 $0F $C2 6 cc-xreg,xmem CMPNLESD
$F2 $0F $C2 7 cc-xreg,xmem CMPORDSD

$F3 $0F $C2 0 cc-xreg,xmem CMPEQSS
$F3 $0F $C2 1 cc-xreg,xmem CMPLTSS
$F3 $0F $C2 2 cc-xreg,xmem CMPLESS
$F3 $0F $C2 3 cc-xreg,xmem CMPUNORDSS
$F3 $0F $C2 4 cc-xreg,xmem CMPNEQSS
$F3 $0F $C2 5 cc-xreg,xmem CMPNLTSS
$F3 $0F $C2 6 cc-xreg,xmem CMPNLESS
$F3 $0F $C2 7 cc-xreg,xmem CMPORDSS

$66 $0F $2F xreg,xmem COMISD
 -1 $0F $2F xreg,xmem COMISS
$F3 $0F $E6 xreg,xmem CVTDQ2PD
 -1 $0F $5B xreg,xmem CVTDQ2PS
$F2 $0F $E6 xreg,xmem CVTPD2DQ
$66 $0F $5A xreg,xmem CVTPD2PS
$66 $0F $5B xreg,xmem CVTPS2DQ
 -1 $0F $5A xreg,xmem CVTPS2PD

$F2 $0F $2D reg32/64,xmem CVTSD2SI
$F2 $0F $5A     xreg,xmem CVTSD2SS
$F2 $0F $2A xreg,r/m32/64 CVTSI2SD
$F3 $0F $2A xreg,r/m32/64 CVTSI2SS
$F3 $0F $5A     xreg,xmem CVTSS2SD
$F3 $0F $2D reg32/64,xmem CVTSS2SI
$66 $0F $E6     xreg,xmem CVTTPD2PQ
$F3 $0F $5B     xreg,xmem CVTTPS2DQ
$F2 $0F $2C reg32/64,xmem CVTTSD2SI
$F3 $0F $2C reg32/64,xmem CVTTSS2SI

$66 $0F $5E xreg,xmem DIVPD
 -1 $0F $5E xreg,xmem DIVPS
$F2 $0F $5E xreg,xmem DIVSD
$F3 $0F $5E xreg,xmem DIVSS

 -1 $0F $A0 0 $20 op3-memsz FXSAVE
 -1 $0F $A0 0 $40 op3-memsz FXSAVE64
 -1 $0F $AE 2 $20 op3-memsz LDMXCSR

$66 $0F $F7 xreg,xreg MASKMOVDQU
$66 $0F $5F xreg,xmem MAXPD
 -1 $0F $5F xreg,xmem MAXPS
$F2 $0F $5F xreg,xmem MAXSD
$F3 $0F $5F xreg,xmem MAXSS
$66 $0F $5D xreg,xmem MINPD
 -1 $0F $5D xreg,xmem MINPS
$F2 $0F $5D xreg,xmem MINSD
$F3 $0F $5D xreg,xmem MINSS
$66 $0F $28 $29 xmem,xmem MOVAPD
 -1 $0F $28 $29 xmem,xmem MOVAPS
$66 $0F $6E $7E $20 xr/r/m,xr/r/m MOVD
$66 $0F $6E $7E $40 gen-movq MOVQ
\ -1 $0F $7E $D6 xmem,xmem MOVQ-XM
$66 $0F $6F $7F xmem,xmem MOVDQA
$F3 $0F $6F $7F xmem,xmem MOVDQU
 -1 $0F $12 xreg,xreg MOVHLPS
$66 $0F $16 $17 $40 xr/r/m,xr/r/m MOVHPD
 -1 $0F $16 $17 $20 xr/r/m,xr/r/m MOVHPS
 -1 $0F $16 xreg,xreg MOVLHPS
$66 $0F $12 $13 $40 xr/r/m,xr/r/m MOVLPD
 -1 $0F $12 $13 $20 xr/r/m,xr/r/m MOVLPS
$66 $0F $50 reg,xreg MOVMSKPD
 -1 $0F $50 reg,xreg MOVMSKPS
$66 $0F $E7 mem,xreg MOVNTDQ
$66 $0F $2B mem,xreg MOVNTPD
 -1 $0F $E7 mem,xreg MOVNTPS
$F2 $0F $10 $11 gen-movsd MOVSD
$F3 $0F $10 $11 xmem,xmem MOVSS
$66 $0F $10 $11 xmem,xmem MOVUPD
 -1 $0F $10 $11 xmem,xmem MOVUPS
$66 $0F $59 xreg,xmem MULPD
 -1 $0F $59 xreg,xmem MULPS
$F2 $0F $59 xreg,xmem MULSD
$F3 $0F $59 xreg,xmem MULSS

$66 $0F $56 xreg,xmem ORPD
 -1 $0F $f6 xreg,xmem ORPS

$66 $0F $63 xreg,xmem PACKSSWB
$66 $0F $6B xreg,xmem PACKSSDW
$66 $0F $67 xreg,xmem PACKUSWB
$66 $0F $FC xreg,xmem PADDB
$66 $0F $FD xreg,xmem PADDW
$66 $0F $FE xreg,xmem PADDD
$66 $0F $D4 xreg,xmem PADDQ
$66 $0F $DC xreg,xmem PADDUSB
$66 $0F $DD xreg,xmem PADDUSW
$66 $0F $DB xreg,xmem PAND
$66 $0F $DF xreg,xmem PANDN
$66 $0F $E0 xreg,xmem PAVGB
$66 $0F $E3 xreg,xmem PAVGW
$66 $0F $74 xreg,xmem PCMPEQB
$66 $0F $75 xreg,xmem PCMPEQW
$66 $0F $76 xreg,xmem PCMPEQD
$66 $0F $64 xreg,xmem PCMPGTB
$66 $0F $65 xreg,xmem PCMPGTW
$66 $0F $66 xreg,xmem PCMPGTD
$66 $0F $C4 xreg,r32/m16,imm8 PINSRW
$66 $0F $F5 xreg,xmem PMADDWD
$66 $0F $EE xreg,xmem PMAXSW
$66 $0F $DE xreg,xmem PMAXUB
$66 $0F $EA xreg,xmem PMINSW
$66 $0F $DA xreg,xmem PMINUB
$66 $0F $E4 xreg,xmem PMULHUW
$66 $0F $E5 xreg,xmem PMULHW
$66 $0F $D5 xreg,xmem PMULLW
$66 $0F $F4 xreg,xmem PMULUDQ
$66 $0F $EB xreg,xmem POR
$66 $0F $F6 xreg,xmem PSADBWW
$66 $0F $70 xreg,xmem,imm8 PSHUFD
$F3 $0F $70 xreg,xmem,imm8 PSHUFHW
$F2 $0F $70 xreg,xmem,imm8 PSHUFLW
$66 $0F $73 7 xreg,imm8 PSLLDQ
$66 $0F $F1 $71 6 xreg,xmem/imm8 PSLLW
$66 $0F $F2 $72 6 xreg,xmem/imm8 PSLLD
$66 $0F $F3 $73 6 xreg,xmem/imm8 PSLLQ
$66 $0F $E1 $71 4 xreg,xmem/imm8 PSRAW
$66 $0F $E2 $72 4 xreg,xmem/imm8 PSRAD
$66 $0F $73 3 xreg,imm8 PSRLDQ
$66 $0F $D1 $71 2 xreg,xmem/imm8 PSRLW
$66 $0F $D2 $72 2 xreg,xmem/imm8 PSRLD
$66 $0F $D3 $73 2 xreg,xmem/imm8 PSRLQ
$66 $0F $F8 xreg,xmem PSUBB
$66 $0F $F9 xreg,xmem PSUBW
$66 $0F $FA xreg,xmem PSUBD
$66 $0F $FB xreg,xmem PSUBQ
$66 $0F $E8 xreg,xmem PSUBSB
$66 $0F $E9 xreg,xmem PSUBSW
$66 $0F $D8 xreg,xmem PSUBUSB
$66 $0F $D9 xreg,xmem PSUBUSW
$66 $0F $68 xreg,xmem PUNPCKHBW
$66 $0F $69 xreg,xmem PUNPCKHWD
$66 $0F $6A xreg,xmem PUNPCKHDQ
$66 $0F $6D xreg,xmem PUNPCKHQDQ
$66 $0F $60 xreg,xmem PUNPCKLBW
$66 $0F $61 xreg,xmem PUNPCKLWD
$66 $0F $62 xreg,xmem PUNPCKLDQ
$66 $0F $6C xreg,xmem PUNPCKLQDQ
$66 $0F $EF xreg,xmem PXOR

 -1 $0F $53 xreg,xmem RCPPS
$F3 $0F $53 xreg,xmem RCPSS
 -1 $0F $52 xreg,xmem RSQRTPS
$F3 $0F $52 xreg,xmem RSQRTSS

$66 $0F $C6 xreg,xmem,imm8 SHUFPD
 -1 $0F $C6 xreg,xmem,imm8 SHUFPS
$66 $0F $51 xreg,xmem SQRTPD
 -1 $0F $51 xreg,xmem SQRTPS
$F2 $0F $51 xreg,xmem SQRTSD
$F3 $0F $51 xreg,xmem SQRTSS
 -1 $0F $AE 3 $20 op3-memsz STMXCSR
$66 $0F $5C xreg,xmem SUBPD
 -1 $0F $5C xreg,xmem SUBPS
$F2 $0F $5C xreg,xmem SUBSD
$F3 $0F $5C xreg,xmem SUBSS

$66 $0F $2E xreg,xmem UCOMISD
 -1 $0F $2E xreg,xmem UCOMISS
$66 $0F $15 xreg,xmem UNPCKHPD
 -1 $0F $15 xreg,xmem UNPCKHPS
$66 $0F $14 xreg,xmem UNPCKLPD
 -1 $0F $14 xreg,xmem UNPCKLPS

$66 $0F $57 xreg,xmem XORPD
 -1 $0F $57 xreg,xmem XORPS

\ ****************************
\ Assembler words for the user
\ ****************************

decimal

\ comma n size mode -- ;
1 0 3 3 reg RAX,   1 1 3 3 reg RCX,   1 2 3 3 reg RDX,   1 3 3 3 reg RBX,
1 0 2 3 reg EAX,   1 1 2 3 reg ECX,   1 2 2 3 reg EDX,   1 3 2 3 reg EBX,
1 0 1 3 reg AX,    1 1 1 3 reg CX,    1 2 1 3 reg DX,    1 3 1 3 reg BX,
1 0 0 3 reg AL,    1 1 0 3 reg CL,    1 2 0 3 reg DL,    1 3 0 3 reg BL,
1 4 3 3 reg RSP,   1 5 3 3 reg RBP,   1 6 3 3 reg RSI,   1 7 3 3 reg RDI,
1 4 2 3 reg ESP,   1 5 2 3 reg EBP,   1 6 2 3 reg ESI,   1 7 2 3 reg EDI,
1 4 1 3 reg SP,    1 5 1 3 reg BP,    1 6 1 3 reg SI,    1 7 1 3 reg DI,
1 4 0 3 reg SPL,   1 5 0 3 reg BPL,   1 6 0 3 reg SIL,   1 7 0 3 reg DIL,

1 8 3 3 reg R8,    1 9 3 3 reg R9,    1 10 3 3 reg R10,  1 11 3 3 reg R11,
1 8 2 3 reg R8D,   1 9 2 3 reg R9D,   1 10 2 3 reg R10D, 1 11 2 3 reg R11D,
1 8 1 3 reg R8W,   1 9 1 3 reg R9W,   1 10 1 3 reg R10W, 1 11 1 3 reg R11W,
1 8 0 3 reg R8L,   1 9 0 3 reg R9L,   1 10 0 3 reg R10L, 1 11 0 3 reg R11L,
1 12 3 3 reg R12,  1 13 3 3 reg R13,  1 14 3 3 reg R14,  1 15 3 3 reg R15,
1 12 2 3 reg R12D, 1 13 2 3 reg R13D, 1 14 2 3 reg R14D, 1 15 2 3 reg R15D,
1 12 1 3 reg R12W, 1 13 1 3 reg R13W, 1 14 1 3 reg R14W, 1 15 1 3 reg R15W,
1 12 0 3 reg R12L, 1 13 0 3 reg R13L, 1 14 0 3 reg R14L, 1 15 0 3 reg R15L,

0 0 3 3 reg RAX    0 1 3 3 reg RCX    0 2 3 3 reg RDX    0 3 3 3 reg RBX
0 0 2 3 reg EAX    0 1 2 3 reg ECX    0 2 2 3 reg EDX    0 3 2 3 reg EBX
0 0 1 3 reg AX     0 1 1 3 reg CX     0 2 1 3 reg DX     0 3 1 3 reg BX
0 0 0 3 reg AL     0 1 0 3 reg CL     0 2 0 3 reg DL     0 3 0 3 reg BL
0 4 3 3 reg RSP    0 5 3 3 reg RBP    0 6 3 3 reg RSI    0 7 3 3 reg RDI
0 4 2 3 reg ESP    0 5 2 3 reg EBP    0 6 2 3 reg ESI    0 7 2 3 reg EDI
0 4 1 3 reg SP     0 5 1 3 reg BP     0 6 1 3 reg SI     0 7 1 3 reg DI
0 4 0 3 reg SPL    0 5 0 3 reg BPL    0 6 0 3 reg SIL    0 7 0 3 reg DIL

0 3 0 3 reg BLreg	\ avoid name conflict with BL=32

0 8 3 3 reg R8     0 9 3 3 reg R9     0 10 3 3 reg R10   0 11 3 3 reg R11
0 8 2 3 reg R8D    0 9 2 3 reg R9D    0 10 2 3 reg R10D  0 11 2 3 reg R11D
0 8 1 3 reg R8W    0 9 1 3 reg R9W    0 10 1 3 reg R10W  0 11 1 3 reg R11W
0 8 0 3 reg R8L    0 9 0 3 reg R9L    0 10 0 3 reg R10L  0 11 0 3 reg R11L
0 12 3 3 reg R12   0 13 3 3 reg R13   0 14 3 3 reg R14   0 15 3 3 reg R15
0 12 2 3 reg R12D  0 13 2 3 reg R13D  0 14 2 3 reg R14D  0 15 2 3 reg R15D
0 12 1 3 reg R12W  0 13 1 3 reg R13W  0 14 1 3 reg R14W  0 15 1 3 reg R15W
0 12 0 3 reg R12L  0 13 0 3 reg R13L  0 14 0 3 reg R14L  0 15 0 3 reg R15L

1 4 0 3 reg AH,    1 5 0 3 reg CH,   1 6 0 3 reg DH,   1 7 0 3 reg BH,
0 4 0 3 reg AH     0 5 0 3 reg CH    0 6 0 3 reg DH    0 7 0 3 reg BH

1 0 1 4 reg ES,    1 1 1 4 reg CS,   1 2 1 4 reg SS,   1 3 1 4 reg DS,
1 4 1 4 reg FS,    1 5 1 4 reg GS,
0 0 1 4 reg ES     0 1 1 4 reg CS    0 2 1 4 reg SS    0 3 1 4 reg DS
0 4 1 4 reg FS     0 5 1 4 reg GS

0 0 0 5 reg #      0 0 0 6 reg ,PC   0 0 0 7 reg SO
0 0 0 5 reg offset	\ -- ; synonym for # (as per spec.)
0 0 0 5 reg seg		\ -- ; another synonym for #

: ptr  (ptr)  ;	\ -- ; specifies direct rather than immediate

: []		\ -- ;
  (ptr)                         \ as above, but also
  got-[] on                     \ say we had one of these
;

: ptr64		\ -- ; specifies direct rather than immediate
  (ptr) dir64-flag on  ;

: [64]		\ -- ;
  (ptr)  dir64-flag on  got-[] on	\ say we had one of these
;

\ type reg# -- ;
1 0 spec-reg DR0   2 0 spec-reg CR0
1 1 spec-reg DR1
1 2 spec-reg DR2   2 2 spec-reg CR2
1 3 spec-reg DR3   2 3 spec-reg CR3
1 6 spec-reg DR6                      3 6 spec-reg TR6
1 7 spec-reg DR7                      3 7 spec-reg TR7

1 0 spec-reg DR0,  2 0 spec-reg CR0,
1 1 spec-reg DR1,
1 2 spec-reg DR2,  2 2 spec-reg CR2,
1 3 spec-reg DR3,  2 3 spec-reg CR3,
1 6 spec-reg DR6,                      3 6 spec-reg TR6,
1 7 spec-reg DR7,                      3 7 spec-reg TR7,

\ index scale -- ;
 0 1 sib [rax*2]    0 2 sib [rax*4]    0 3 sib [rax*8]
 1 1 sib [rcx*2]    1 2 sib [rcx*4]    1 3 sib [rcx*8]
 2 1 sib [rdx*2]    2 2 sib [rdx*4]    2 3 sib [rdx*8]
 3 1 sib [rbx*2]    3 2 sib [rbx*4]    3 3 sib [rbx*8]
\ rsp
 5 1 sib [rbp*2]    5 2 sib [rbp*4]    5 3 sib [rbp*8]
 6 1 sib [rsi*2]    6 2 sib [rsi*4]    6 3 sib [rsi*8]
 7 1 sib [rdi*2]    7 2 sib [rdi*4]    7 3 sib [rdi*8]
 8 1 sib [r8*2]     8 2 sib [r8*4]     8 3 sib [r8*8]
 9 1 sib [r9*2]     9 2 sib [r9*4]     9 3 sib [r9*8]
10 1 sib [r10*2]   10 2 sib [r10*4]   10 3 sib [r10*8]
11 1 sib [r11*2]   11 2 sib [r11*4]   11 3 sib [r11*8]
12 1 sib [r12*2]   12 2 sib [r12*4]   12 3 sib [r12*8]
13 1 sib [r13*2]   13 2 sib [r13*4]   13 3 sib [r13*8]
14 1 sib [r14*2]   14 2 sib [r14*4]   14 3 sib [r14*8]
15 1 sib [r15*2]   15 2 sib [r15*4]   15 3 sib [r15*8]
decimal

\ comma size n --
\ 64 bit
1 3  0 ind [RAX],  1 3  1 ind [RCX],  1 3  2 ind [RDX],  1 3  3 ind [RBX],
1 3  4 ind [RSP],  1 3  5 ind [RBP],  1 3  6 ind [RSI],  1 3  7 ind [RDI],
1 3  8 ind [r8],   1 3  9 ind [r9],   1 3 10 ind [r10],  1 3 11 ind [r11],
1 3 12 ind [r12],  1 3 13 ind [r13],  1 3 14 ind [r14],  1 3 15 ind [r15],

0 3  0 ind [RAX]   0 3  1 ind [RCX]   0 3  2 ind [RDX]   0 3  3 ind [RBX]
0 3  4 ind [RSP]   0 3  5 ind [RBP]   0 3  6 ind [RSI]   0 3  7 ind [RDI]
0 3  8 ind [r8]    0 3  9 ind [r9]    0 3 10 ind [r10]   0 3 11 ind [r11]
0 3 12 ind [r12]   0 3 13 ind [r13]   0 3 14 ind [r14]   0 3 15 ind [r15]

hex

: byte		( -- )  (byte)  ;
: b.		( -- )  (byte)  ;
: word		( -- )  (word)  ;
: worda		( -- )  (word)  ;	\ avoid name conflict with Forth WORD
: w.		( -- )  (word)  ;
: dword		( -- )  (dword)  ;
: dw.		( -- )  (dword)  ;
: qword		( -- )  (qword)  ;
: qw.		( -- )  (qword)  ;
: tbyte		( -- )  (tbyte)  ;
: float		( -- )  (float)  ;
: double	( -- )  (double)  ;
: extended	( -- )  (extended)  ;


: jmp/f         \ seg off -- ; for compatibility with old asm
  0 ?prefix drop                \ do the last one
  (jmp/f)                       \ lay the far jump
;

: jmp           \ addr | seg/off -- ;
  0 ?prefix drop                \ do the last one
  far/near @                    \ if far or near
  if    (jmp/f)                 \ lay far form
  else  (jmp)                   \ or near form
  THEN
;

: call/f        \ seg off -- ; for compatiblity with old asm
  0 ?prefix drop                \ do the last one
  (call/f)
;

: call          \ addr | seg/off -- ; general call
  0 ?prefix drop                \ do the last one
  far/near @
  if    (call/f)                \ do far
  else  (call)                  \ or do near
  THEN
;

hex

: enter         \ imm16 imm8 -- ;
  0 ?prefix drop (enter)
;

: imul          \ ??? -- ;
  0 ?prefix drop (imul)         \ do the last one
;

: esc           \ -- ;
  0 ?prefix drop  (esc)         \ do the last one
;

: in            \ ??? -- ;
  0 ?prefix drop  (in)          \ do the last one
;

: out           \ ??? -- ;
  0 ?prefix drop  (out)		\ do the last one
;

: int           \ n --
  0 ?prefix drop  (int)
;

: mov           \ ??? -- ;
  0 ?prefix drop  (mov)
;

: test          \ ??? -- ;
  0 ?prefix drop  (test)
;

: xchg          \ ??? -- ;
  0 ?prefix drop  (xchg)
;

\ 02e lay-seg cs:
\ 03e lay-seg ds:
\ 026 lay-seg es:
$064 lay-seg fs:
$065 lay-seg gs:
\ 036 lay-seg ss:

: fwait  (fwait)  ;	\ -- ; lays wait opcode

hex

\ comma n size mode -- ;
0 0 1 9 reg ST     1 0 1 9 reg ST,
0 0 1 9 reg ST(0)  1 0 1 9 reg ST(0),
0 1 1 9 reg ST(1)  1 1 1 9 reg ST(1),
0 2 1 9 reg ST(2)  1 2 1 9 reg ST(2),
0 3 1 9 reg ST(3)  1 3 1 9 reg ST(3),
0 4 1 9 reg ST(4)  1 4 1 9 reg ST(4),
0 5 1 9 reg ST(5)  1 5 1 9 reg ST(5),
0 6 1 9 reg ST(6)  1 6 1 9 reg ST(6),
0 7 1 9 reg ST(7)  1 7 1 9 reg ST(7),

\ byte1 byte2 -- ;
D9 0F0 f0op    F2XM1    D9 0E1 f0op FABS     D9 0E0 f0op FCHS
DB 0E2 f0op(w) FCLEX    DB 0E2 f0op FNCLEX   DE 0D9 f0op FCOMPP
D9 0FF f0op    FCOS     D9 0F6 f0op FDECSTP  D9 0F7 f0op FINCSTP
DB 0E3 f0op(w) FINIT    DB 0E3 f0op FNINIT   D9 0E8 f0op FLD1
D9 0E9 f0op    FLDL2T   D9 0EA f0op FLDL2E   D9 0EB f0op FLDPI
D9 0EC f0op    FLDLG2   D9 0ED f0op FLDLN2   D9 0EE f0op FLDZ
D9 0D0 f0op    FNOP     D9 0F3 f0op FPATAN   D9 0F8 f0op FPREM
D9 0F5 f0op    FPREM1   D9 0F2 f0op FPTAN    D9 0FC f0op FRNDINT
D9 0FD f0op    FSCALE   D9 0FE f0op FSIN     D9 0FB f0op FSINCOS
D9 0FA f0op    FSQRT    D9 0E4 f0op FTST     DA 0E9 f0op FUCOMPP
D9 0E5 f0op    FXAM     D9 0F4 f0op FXTRACT  D9 0F1 f0op FYL2X
D9 0F9 f0op    FYL2XP1

DB 0E0 f0op FENI     DB 0E1 f0op FDISI    \ not used in '387

\ byte1 byte2 -- ;
DD 0E0 fs1/0 FUCOM
DD 0E8 fs1/0 FUCOMP
D9 0C8 fs1/0 FXCH

\ byte1 byte2 -- ;
DD 0C0 fs1 FFREE

\ byte32 byte16 reg-mask -- ;
DA DE 00 fm16/32 FIADD
DA DE 02 fm16/32 FICOM
DA DE 03 fm16/32 FICOMP
DA DE 06 fm16/32 FIDIV
DA DE 07 fm16/32 FIDIVR
DA DE 01 fm16/32 FIMUL
DB DF 02 fm16/32 FIST
DA DE 04 fm16/32 FISUB
DA DE 05 fm16/32 FISUBR

\ byte1 byte2 -- ;
0DE 0C0 fs1,s FADDP
0DE 0F8 fs1,s FDIVP
0DE 0F0 fs1,s FDIVRP
0DE 0C8 fs1,s FMULP
0DE 0E8 fs1,s FSUBP
0DE 0E0 fs1,s FSUBRP

\ op1 reg-mask -- ;
0DF 04 fmxx FBLD                \ 10     bytes
0DF 06 fmxx FBSTP               \ 10     bytes
0D9 05 fmxx FLDCW               \  2     bytes
0D9 04 fmxx FLDENV              \ 14/28  bytes
0DD 04 fmxx FRSTOR              \ 94/108 bytes
0DD 06 fmxx FNSAVE              \ 94/108 bytes
0D9 07 fmxx FNSTCW              \  2     bytes
0D9 06 fmxx FNSTENV             \ 14/28  bytes

\ op1 reg-mask -- ;
0DD 06 fmxx(w) FSAVE            \ 94/108 bytes
0D9 07 fmxx(w) FSTCW            \  2     bytes
0D9 06 fmxx(w) FSTENV           \ 14/28  bytes

\ op1 op2 regmask wait? -- ;
DD 0E0 07 -1 fm/ax FSTSW	\ is either DD or DF depending on mode
DD 0E0 07  0 fm/ax FNSTSW

\ op1 reg-mask1 op2 reg-mask2 -- ;
DF 00 DB 05 fm16/32/64 FILD
DF 03 DB 07 fm16/32/64 FISTP

\ op1 op2 op3 op4 op5 reg-mask -- ;
D9 DD DB DD D8 03 07 fm32/64/80/s FSTP
D9 DD DB D9 C0 00 05 fm32/64/80/s FLD

\ op1 op2 op3 reg-mask -- ;
D9 DD D0 02 fm32/64/s FST

\ op32 mask32 op64 mask64 op1st op2st op10 op20 -- ;
D8 02  DC 02  D8 D0  D8 D1  fm/s1/0 FCOM
D8 03  DC 03  D8 D8  D8 D9  fm/s1/0 FCOMP

\ reg-mask op2 -- ;
00 C0 fm/ss/0 FADD
06 F8 fm/ss/0 FDIV
07 F0 fm/ss/0 FDIVR
01 C8 fm/ss/0 FMUL
04 E8 fm/ss/0 FSUB		\ should be E8 ; SFP002
05 E0 fm/ss/0 FSUBR		\ should be E0 ; SFP002

hex

( 037 0op1 AAA   03f 0op1 AAS  )  0f8 0op1 CLC     09b 0op1 WAIT
0fc 0op1 CLD     0fa 0op1 CLI     0f5 0op1 CMC     0d7 0op1 XLATB
027 0op1 DAA     02f 0op1 DAS     06c 0op1 INSB    0d7 0op1 XLAT
0f4 0op1 HLT   ( 0ce 0op1 INTO )  0cf 0op1 IRET    09f 0op1 LAHF
0f0 0op1 LOCK
090 0op1 NOP
0f3 0op1 REP     0cf 0op1 IRETD   0c9 0op1 LEAVE
0f3 0op1 REPE    0f3 0op1 REPZ    0f2 0op1 REPNE   0f2 0op1 REPNZ
09e 0op1 SAHF    0f9 0op1 STC
0fd 0op1 STD     0fb 0op1 STI
0C3 0op1 NEXT,	\ macro for plain RET
: NEXT NEXT, ;

0a6 0op1    CMPSB       0ac 0op1    LODSB   0a4 0op1    MOVSB
0a7 0op1-16 CMPSW       0ad 0op1-16 LODSW   0a5 0op1-16 MOVSW
0a7 0op1-32 CMPSD       0ad 0op1-32 LODSD   ( 0a5 0op1-32 MOVSD )
0a7 0op1-64 CMPSQ       0ad 0op1-64 LODSQ   0a5 0op1-64 MOVSQ

06e 0op1    OUTSB       0ae 0op1    SCASB   0aa 0op1    STOSB
06f 0op1-16 OUTSW       0af 0op1-16 SCASW   0ab 0op1-16 STOSW
06f 0op1-32 OUTSD       0af 0op1-32 SCASD   0ab 0op1-32 STOSD
                        0af 0op1-64 SCASQ   0ab 0op1-64 STOSQ

06d 0op1-16 INSW        098 0op1-16 CBW     099 0op1-16 CWD
06d 0op1-32 INSD        098 0op1-32 CWDE    099 0op1-32 CDQ
$098 0op1-64 CDQE
$099 0op1-64 CQO

\ 0d5 0a 0op2 AAD
\ 0d4 0a 0op2 AAM
$0f $05 0op2 SYSCALL
$0f $34 0op2 SYSENTER
$0f $06 0op2 CLTS
$0F $A2 0op2 CPUID
$0F $31 0op2 RDTSC
$0F $77 0op2 EMMS

$0F $AE $38 1op2-m8 CLFLUSH
$0f $ae $e8 1op3 LFENCE
$0f $ae $f0 1op3 MFENCE
$0f b8 F3,2op-r,r/m POPCNT
$0f bc F3,2op-r,r/m TZCNT
$0f bd F3,2op-r,r/m LZCNT
$38 $f0 0F,2op-r/m,r/m MOVBE

hex

010 014 080 010 3modes   ADC      000 004 080 000 3modes   ADD
020 024 080 020 3modes/l AND      038 03C 080 038 3modes   CMP
008 00C 080 008 3modes/l OR       018 01C 080 018 3modes   SBB
028 02C 080 028 3modes   SUB      030 034 080 030 3modes/l XOR
				  030 034 080 030 3modes/l XORa
				  008 00C 080 008 3modes/l ORa
				  020 024 080 020 3modes/l ANDa
( 048 ) 0FE 008 inc/dec DEC
( 040 ) 0FE 000 inc/dec INC

030 mul/div DIV         038 mul/div IDIV
020 mul/div MUL
018 mul/div NEG         010 mul/div NOT		010 mul/div NOTa

hex
\ o16 -- ;
077 j/lp JA     077 j/lp JNBE   073 j/lp JAE   073 j/lp JNB
072 j/lp JB     072 j/lp JNAE   076 j/lp JBE   076 j/lp JNA
0E3 j/lp JCXZ   074 j/lp JE     074 j/lp JZ    07F j/lp JG
07F j/lp JNLE   07D j/lp JGE    07D j/lp JNL   07C j/lp JL
07C j/lp JNGE   07E j/lp JLE    07E j/lp JNG   0e3 j/lp JECXZ	0e3 j/lp JRCXZ
075 j/lp JNE    075 j/lp JNZ    071 j/lp JNO   07B j/lp JNP
07B j/lp JPO    079 j/lp JNS    070 j/lp JO    07A j/lp JP
07A j/lp JPE    078 j/lp JS     072 j/lp JC    073 j/lp JNC

0E2 j/lp LOOP   0E1 j/lp LOOPZ  0E1 j/lp LOOPE 0E0 j/lp LOOPNZ
0E0 j/lp LOOPNE

0E2 j/lp LOOPa	\ for LOOP,

hex

$00F $008 0op2 INVD
$0C8 1op-r32 BSWAP
$0B0 2op-r/m,r CMPXCHG
$0C0 2op-r/m,r XADD

\ 0C5 reg,mem LDS
0f b2 reg,mem(2) LSS
\ 0C4 reg,mem LES
0f b4 reg,mem(2) LFS
0f b5 reg,mem(2) LGS

08D reg,mem LEA

0a1 0a9 08F 000 058 007 pop/push POP
0a0 0a8 0FF 030 050 006 pop/push PUSH

010 shifts RCL          018 shifts RCR
000 shifts ROL          008 shifts ROR
000 shifts ROLa         008 shifts RORa

038 shifts SAR          020 shifts SAL
020 shifts SHL          028 shifts SHR

0c2 0CA returns RET

: ret/f         \ [imm] -- ;
  0CA rets
;

\ byte1 byte2 -- ;
0f bc bit-scan BSF
0f bd bit-scan BSR

\ /d byte1 byte2(reg) byte2(imm) /r(byte2) -- ;
04 0f a3 ba 4 bit-test BT
07 0f bb ba 7 bit-test BTC
06 0f b3 ba 6 bit-test BTR
05 0f ab ba 5 bit-test BTS

0f 02 2op-r,r/m LAR
0f 03 2op-r,r/m LSL
0f 0be 0bf mov(x)x MOVSX
0f 0b6 0b7 mov(x)x MOVZX
$63 reg,mem(sz) MOVSXD

\ SFP011...
0F 40 reg,mem(3) CMOVO
0F 41 reg,mem(3) CMOVNO
0F 42 reg,mem(3) CMOVB
0F 42 reg,mem(3) CMOVC
0F 42 reg,mem(3) CMOVNAE
0F 43 reg,mem(3) CMOVAE
0F 43 reg,mem(3) CMOVNB
0F 43 reg,mem(3) CMOVNC
0F 44 reg,mem(3) CMOVE
0F 44 reg,mem(3) CMOVZ
0F 45 reg,mem(3) CMOVNE
0F 45 reg,mem(3) CMOVNZ
0F 46 reg,mem(3) CMOVBE
0F 46 reg,mem(3) CMOVNA
0F 47 reg,mem(3) CMOVNBE
0F 47 reg,mem(3) CMOVA
0F 48 reg,mem(3) CMOVS
0F 49 reg,mem(3) CMOVNS
0F 4A reg,mem(3) CMOVP
0F 4A reg,mem(3) CMOVPE
0F 4B reg,mem(3) CMOVNP
0F 4B reg,mem(3) CMOVNPO
0F 4C reg,mem(3) CMOVL
0F 4C reg,mem(3) CMOVNGE
0F 4D reg,mem(3) CMOVNL
0F 4D reg,mem(3) CMOVGE
0F 4E reg,mem(3) CMOVLE
0F 4E reg,mem(3) CMOVNG
0F 4F reg,mem(3) CMOVNLE
0F 4F reg,mem(3) CMOVG
\ ...SFP011

\ 1 061 0op16/32 POPA
\ 2 061 0op16/32 POPAD
1 09d 0op16/32 POPF
\ 2 09d 0op16/32 POPFD
\ 1 060 0op16/32 PUSHA
\ 2 060 0op16/32 PUSHAD
1 09c 0op16/32 PUSHF
\ 2 09c 0op16/32 PUSHFD

090 set SETO
091 set SETNO
092 set SETB            092 set SETC            092 set SETNAE
093 set SETAE           093 set SETNB           093 set SETNC
094 set SETE            094 set SETZ
095 set SETNE           095 set SETNZ
096 set SETBE           096 set SETNA
097 set SETA            097 set SETNBE
098 set SETS
099 set SETNS
09a set SETP            09a set SETPE
09b set SETNP           09b set SETPO
09c set SETL            09c set SETNGE
09d set SETGE           09d set SETNL
09e set SETLE           09e set SETNG
09f set SETG            09f set SETNLE

0f 0a4 d-shift SHLD
0f 0ac d-shift SHRD

0f 01 02 load-table LGDT
0f 01 03 load-table LIDT
0f 01 00 load-table SGDT
0f 01 01 load-table SIDT

0f 00 02 2op-r/m LLDT
0f 01 06 2op-r/m LMSW
0f 00 03 2op-r/m LTR
0f 00 00 2op-r/m SLDT
0f 01 04 2op-r/m SMSW
0f 00 01 2op-r/m STR
0f 00 04 2op-r/m VERR
0f 00 05 2op-r/m VERW

: ,  (,)  ;	\ --
\ define comma separator for the assembler

: ,a  (,)  ;	\ --
\ define comma separator for the assembler

: d16:          \ -- ;
  d-override on			\ then set override
;

: a16:          \ -- ;
  a-override on			\ then set override
;

: far           \ -- ;
  far/near on
;

: near          \ -- ;
  far/near off
;

: short         \ -- ;
  short/long off
;

: long          \ -- ;
  short/long on
;

(*
alt (for modern CPUs, same for Intel & AMD):

 1 90
 2 6690
 3 0f1f00                          nopl (%[re]ax)
 4 0f1f4000                        nopl 0(%[re]ax)
 5 0f1f440000                      nopl 0(%[re]ax,%[re]ax,1)
 6 660f1f440000                    nopw 0(%[re]ax,%[re]ax,1)
 7 0f1f8000000000                  nopl 0L(%[re]ax)
 8 0f1f840000000000                nopl 0L(%[re]ax,%[re]ax,1)
 9 660f1f840000000000              nopw 0L(%[re]ax,%[re]ax,1)
10 662e0f1f840000000000            nopw %cs:0L(%[re]ax,%[re]ax,1)
*)
: nop1		\ --
  0 ?prefix drop  $90 C,  ;
: nop2		\ --
  0 ?prefix drop  $66 C,  $90 C,  ;
: nop3		\ --
  0 ?prefix drop  $0F C,  $1F C,  $00 C,  ;
: nop4		\ --
  0 ?prefix drop  $0F C,  $1F C,  $40 C,  $00 C,  ;
: nop5		\ --
  0 ?prefix drop  $0F C,  $1F C,  $44 C,  $00 C,  $00 C,  ;
: nop6		\ --
  0 ?prefix drop  $66 C,  $0F C,  $1F C,  $44 C,  $00 C,  $00 C,  ;
: nop7		\ --
  0 ?prefix drop  $0F C,  $1F C,  $80 C,  $00 L,  ;
: nop8		\ --
  0 ?prefix drop  $0F C,  $1F C,  $84 C,  $00 C,  $00 L,  ;
: nop9		\ --
  0 ?prefix drop  $66 C,  $0F C,  $1F C,  $84 C,  $00 C,  $00 L,  ;
: nop10		\ --
  0 ?prefix drop  $66 C,  $2E C,  $0F C,  $1F C,  $84 C,  $00 C,  $00 L,  ;


\ ***********************
\ *S Assembler structures
\ ***********************
\ *P The assembler includes structure words modelled after the
\ ** usual Forth structures

0 constant con-flg		\ -- 0 ; address structure flag

: ?>mark        \ -- as-flag as-orig
\ lay forward branch offset after opcode; patched up later
  con-flg HERE  0 C,
;

: ?>resolve     \ asflag as-orig --
\ patch up forward branch offset after opcode
  HERE over 1+ -  dup ?short
  swap C!  con-flg ?pairs
;

: ?<mark        \ -- as-flag as-dest
\ Mark start of loop (destination of backward branch).
  con-flg  HERE
;

: ?<resolve     \ as-flag as-dest --
\ lay offset of backward branch.
  HERE 1+ -  dup ?short  C,
  con-flg  ?pairs
;

hex

75 constant Z,     74 constant NZ,    79 constant S,
78 constant NS,    7D constant L,     7C constant GE,
7F constant LE,    7E constant G,     73 constant CY,
72 constant NC,    77 constant BE,    76 constant A,
71 constant O,     70 constant NO,    7A constant PO,
7B constant PE,    72 constant AE,    73 constant B,
E3 constant NCXZ,  72 constant NCY,   EB constant NV, ( never )

: if,		\ cond -- as-flag as-orig
  >r  null-op  r> C, ?>mark		\ preserve condition, and do last opcode
;

: then,		\ as-flag as-orig --
  null-op  ?>resolve
;

: endif,	\ as-flag as-orig --
  null-op  ?>resolve
;

: else,		\ as-flag1 as-orig1 -- as-flag2 as-orig2
  null-op  $0EB if,  2swap  then,	\ magic number for JMP rel8
;

: begin,	\ -- as-flag as-dest
  null-op  ?<mark
;

: until,	\ as-flag as-dest cond --
  >r  null-op  r> C,  ?<resolve
;

: again,	\ as-flag as-dest --
  null-op  $0EB until,
;

: while,	\ as-flag as-dest cond -- as-flag as-orig as-flag as-dest
  null-op  if, 2swap
;

: repeat,	\ as-flag as-orig as-flag as-dest --
  null-op  again,  then,
;

: times,	\ n -- dest
  >r  mov rcx, # r>  null-op  HERE	\ points after  mov  rcx, # n
;

: loop,         \ dest -- ;
  loopa                          \ synonym
;

decimal

0 VALUE TEB_CLEAN

: CODL
	null-op
\+ :#THS   :#THS
	TEXEC_?
	IF
[IFDEF] TEXEC_BUF
	>IN M@ >R
	PARSE-NAME
 SFIND

	IF
			TEB_CLEAN CELL+ TO TEB_CLEAN
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
[THEN]  THEN
	HEADER
;

0 VALUE SAVELAST

: ASM_END
   null-op asm-off \	PREVIOUS
\+ SHERE-TO-TAB   SHERE-TO-TAB

;

EXPORT

: VFX_ASM_BIG
\+ SHERE-TO-TAB  SHERE-TO-TAB
\	ALSO asm-access
	0 TO TEB_CLEAN
	CURRENT M@ M@ TO SAVELAST
;

: VFXCODE
    asm-on set-null
    CODL
    VFX_ASM_BIG \    (CODE)
  ;

: END-CODE ASM_END ;

;MODULE
