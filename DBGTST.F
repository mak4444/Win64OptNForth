

REQUIRE <DBG> ~mak\lib\DBGWC.F

<DBG> 

 ~mak/amd64/hasmx64.fth

\ REQUIRE DISP ~mak/amd64/disasm.fs
REQUIRE DISA ~mak\amd64\hdasmx64.fth 


\ : WWW ."  QWERTY" ;

[undefined] equ [if]
: equ constant  ;
[then]

$55 equ imm8
$55AA equ imm16
$12345678 equ imm32

variable varFooBar

VFXCODE adc1

  adc	al, # imm8
  adc	ax, # imm16
  adc	eax, # imm32
  adc	rax, # imm32

  adc	cl, # imm8
  adc	byte 0 [rbx], # imm8
  adc	byte 0 [r10], # imm8

  adc	cx, # imm16
  adc	word 0 [rbx], # imm16
  adc	word 0 [r10], # imm16

  adc	ecx, # imm32
  adc	dword 0 [rbx], # imm32
  adc	dword 0 [r10], # imm32

  adc	rcx, # imm32
  adc	qword 0 [rbx], # imm32
  adc	qword 0 [r10], # imm32

  adc	cl, r12l
  adc	byte 0 [rbx], r12l
  adc	byte 0 [r10], r12l

  adc	cx, r10w
  adc	word 0 [rbx], r12w
  adc	word 0 [r10], r12w

  adc	ecx, r12d
  adc	dword 0 [rbx], r12d
  adc	dword 0 [r10], r12d

  adc	rcx, r12
  adc	qword 0 [rbx], r12
  adc	qword 0 [r10], r12

  adc	cl, r12l
  adc	r12l, 0 [rbx]
  adc	r12l, 0 [r10]

  adc	cx, r10w
  adc	cx, word 0 [rbx]
  adc	cx, word 0 [r10]

  adc	ecx, r12d
  adc	ecx, dword 0 [rbx]
  adc	ecx, dword 0 [r10]

  adc	rcx, r12
  adc	r12, 0 [rbx]
  adc	r12, 0 [r10]

  adc	r12l, 0 [rbx] [rsi]
  adc	r12l, imm8 [r10] [rsi]
  adc	cx, word 0 [rbx] [rsi]
  adc	cx, word imm32 [r10] [rsi]
  adc	ecx, r12d
  adc	ecx, dword 0 [rbx] [rsi]
  adc	ecx, dword imm8 [r10] [rsi]
  adc	r12, 0 [rbx] [rsi]
  adc	r12, imm8 [r10] [rsi]

  adc	r12l, 0 [rbx] [rsi]
  adc	r12l, imm8 [r10] [rsi]

  next,
end-code

vfxcode cmove1	\ src dest len --
\ load parameters
	mov	r8, rsi
	mov	r9, rdi
	mov	rsi, cell [rbp]		\ src
	mov	rdi, 0 [rbp]		\ dest
	mov	rcx, rbx		\ len
	jrcxz	l$1
	rep movsb
l$1:
	mov	rdi, r9
	mov	rsi, r8
	mov	rbx, 2 cells [rbp]
	lea	rbp, 3 cells [rbp]
	next,
end-code

variable varFooBar



VFXCODE WWW
	 cmova  rax, ptr varFooBar
\	POP     DWord 0 [RBP]

        MOV    RAX, varFooBar []
        MOV    varFooBar [] , RAX
	LEA    RBP, -8 [RBP]
	MOV    0 [RBP], RAX
	LEA    RAX, 8 [RSP]
  ADD     RBX, RSP
  AND     RBX, # 0F
 POP RAX
 NOP
  RET

END-CODE

.S

: WWW1   [  $9A C, $40 C, 0 C, 0 W, 0 W,  ]  ;



7 9

' WWW1 F7_ED DISA
