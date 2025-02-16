BITS 64

%define align(n,r) (((n+(r-1))/r)*r)

SSS_size equ 0x20000
; DOS Header
    dw 'MZ'                 ; e_magic
    dw 0                    ; [UNUSED] e_cblp
    dw 0                    ; [UNUSED] c_cp
    dw 0                    ; [UNUSED] e_crlc
    dw 0                    ; [UNUSED] e_cparhdr
    dw 0                    ; [UNUSED] e_minalloc
    dw 0                    ; [UNUSED] e_maxalloc
    dw 0                    ; [UNUSED] e_ss
    dw 0                    ; [UNUSED] e_sp
    dw 0                    ; [UNUSED] e_csum
    dw 0                    ; [UNUSED] e_ip
    dw 0                    ; [UNUSED] e_cs
    dw 0                    ; [UNUSED] e_lfarlc
    dw 0                    ; [UNUSED] e_ovno
    times 4 dw 0            ; [UNUSED] e_res
    dw 0                    ; [UNUSED] e_oemid
    dw 0                    ; [UNUSED] e_oeminfo
    times 10 dw 0           ; [UNUSED] e_res2
    dd pe_hdr               ; e_lfanew

; DOS Stub
;    times 8 dq 0            ; [UNUSED] DOS Stub

; Rich Header
;    times 8 dq 0            ; [UNUSED] Rich Header

pe_hdr:
    dw 'PE', 0              ; Signature

; Image File Header
    dw 0x8664               ; Machine
    dw 0x01                 ; NumberOfSections
    dd 0                    ; [UNUSED] TimeDateStamp
    dd 0                    ; PointerToSymbolTable
    dd 0                    ; NumberOfSymbols
    dw opt_hdr_size         ; SizeOfOptionalHeader
    dw 0x2F                 ; Characteristics

; Optional Header, COFF Standard Fields
opt_hdr:
    dw 0x020b               ; Magic (PE32+)
    db 0x0e                 ; MajorLinkerVersion
    db 0x16                 ; MinorLinkerVersion
    dd code_size            ; SizeOfCode
    dd 0                    ; SizeOfInitializedData
    dd 0                    ; SizeOfUninitializedData
    dd entry                ; AddressOfEntryPoint
;    dd iatbl                ; BaseOfCode
    dd 0                ; BaseOfCode

; Optional Header, NT Additional Fields
;    dq 0x000140000000       ; ImageBase
    dq 0x400000		; ImageBase
    dd 0x10                 ; SectionAlignment
    dd 0x10                 ; FileAlignment
    dw 0x06                 ; MajorOperatingSystemVersion
    dw 0                    ; MinorOperatingSystemVersion
    dw 0                    ; MajorImageVersion
    dw 0                    ; MinorImageVersion
    dw 0x06                 ; MajorSubsystemVersion
    dw 0                    ; MinorSubsystemVersion
    dd 0                    ; Reserved1
    dd file_size            ; SizeOfImage
    dd hdr_size             ; SizeOfHeaders
    dd 0                    ; CheckSum
    dw 0x03                 ; Subsystem (Windows)
    dw 0 ; x8160               ; DllCharacteristics
    dq 0x100000             ; SizeOfStackReserve
    dq 0x1000               ; SizeOfStackCommit
    dq 0x100000             ; SizeOfHeapReserve
    dq 0 ; x1000               ; SizeOfHeapCommit
    dd 0                    ; LoaderFlags
    dd 0x10                 ; NumberOfRvaAndSizes

; Optional Header, Data Directories
    dd 0                    ; Export, RVA
    dd 0                    ; Export, Size
    dd itbl                 ; Import, RVA
    dd itbl_size            ; Import, Size
    dd 0                    ; Resource, RVA
    dd 0                    ; Resource, Size
    dd 0                    ; Exception, RVA
    dd 0                    ; Exception, Size
    dd 0                    ; Certificate, RVA
    dd 0                    ; Certificate, Size
    dd 0                    ; Base Relocation, RVA
    dd 0                    ; Base Relocation, Size
    dd 0                    ; Debug, RVA
    dd 0                    ; Debug, Size
    dd 0                    ; Architecture, RVA
    dd 0                    ; Architecture, Size
    dd 0                    ; Global Ptr, RVA
    dd 0                    ; Global Ptr, Size
    dd 0                    ; TLS, RVA
    dd 0                    ; TLS, Size
    dd 0                    ; Load Config, RVA
    dd 0                    ; Load Config, Size
    dd 0                    ; Bound Import, RVA
    dd 0                    ; Bound Import, Size
    dd 0 ; iatbl                ; IAT, RVA
    dd 0 ; iatbl_size           ; IAT, Size
    dd 0                    ; Delay Import Descriptor, RVA
    dd 0                    ; Delay Import Descriptor, Size
    dd 0                    ; CLR Runtime Header, RVA
    dd 0                    ; CLR Runtime Header, Size
    dd 0                    ; Reserved, RVA
    dd 0                    ; Reserved, Size

opt_hdr_size equ $-opt_hdr

; Section Table
    section_name db '.flat'     ; Name
    times 8-($-section_name) db 0
    dd sect_size            ; VirtualSize
    dd 0 ; iatbl                ; VirtualAddress
    dd flat_code_size            ; SizeOfRawData
    dd 0 ; iatbl                ; PointerToRawData
    dd 0                    ; PointerToRelocations
    dd 0                    ; PointerToLinenumbers
    dw 0                    ; NumberOfRelocations
    dw 0                    ; NumberOfLinenumbers
;    dd 0x60000020           ; Characteristics
    dd 0xE0000060           ; Characteristics

hdr_size equ $-$$

code:
; Symbol
MessageBoxW_n:
    dw 0x0              ; [UNUSED] Function Order
    db 'MessageBoxW', 0     ; Function Name

SetFocus_n:
    dw 0x0              ; [UNUSED] Function Order
    db 'SetFocus', 0     ; Function Name

; Import Name Table
intbl:
    dq MessageBoxW_n
    dq SetFocus_n
    dq 0

; Import Address Directory
iatbl:
MessageBoxW_b:
    dq MessageBoxW_n
SetFocus_b:
    dq SetFocus_n
    dq 0


dll_name:
    db 'USER32.dll', 0

; Symbol

 CloseHandle_n:		dw 0x0
    db 'CloseHandle', 0        
 CreateFileA_n:		dw 0x0
    db 'CreateFileA', 0        
 CreateThread_n:	dw 0x0
    db 'CreateThread', 0       
 ExitProcess_n:		dw 0x0
    db 'ExitProcess', 0        
 GetCommandLineA_n:	dw 0x0
    db 'GetCommandLineA', 0    
 GetProcAddress_n:	dw 0x0
   db 'GetProcAddress', 0     
 GetStdHandle_n:	dw 0x0
    db 'GetStdHandle', 0       
 LoadLibraryA_n:	dw 0x0
    db 'LoadLibraryA', 0       
 ReadConsoleInputA_n:	dw 0x0
    db 'ReadConsoleInputA', 0  
 ReadFile_n:		dw 0x0
    db 'ReadFile', 0           
 SetFilePointer_n:	dw 0x0
    db 'SetFilePointer', 0     
 Sleep_n:		dw 0x0
    db 'Sleep', 0              
 VirtualAlloc_n:	dw 0x0
    db 'VirtualAlloc', 0       
 VirtualFree_n:		dw 0x0
    db 'VirtualFree', 0        
 WriteFile_n:		dw 0x0
    db 'WriteFile', 0          


; Import Name Table
intbl_k:
  CloseHandle_a:         dq CloseHandle_n
  CreateFileA_a:         dq CreateFileA_n
  CreateThread_a:        dq CreateThread_n
  ExitProcess_a:         dq ExitProcess_n
  GetCommandLineA_a:     dq GetCommandLineA_n
  GetProcAddress_a:      dq GetProcAddress_n
  GetStdHandle_a:        dq GetStdHandle_n
  LoadLibraryA_a:        dq LoadLibraryA_n
  ReadConsoleInputA_a:   dq ReadConsoleInputA_n
  ReadFile_a:            dq ReadFile_n
  SetFilePointer_a:      dq SetFilePointer_n
  Sleep_a:               dq Sleep_n
  VirtualAlloc_a:        dq VirtualAlloc_n
  VirtualFree_a:         dq VirtualFree_n
  WriteFile_a:           dq WriteFile_n
 dq 0
; Import Address Directory
iatbl_k:
  CloseHandle_b:        dq CloseHandle_n        
  CreateFileA_b:        dq CreateFileA_n        
  CreateThread_b:       dq CreateThread_n       
  ExitProcess_b:        dq ExitProcess_n        
  GetCommandLineA_b:    dq GetCommandLineA_n    
  GetProcAddress_b:     dq GetProcAddress_n     
  GetStdHandle_b:       dq GetStdHandle_n       
  LoadLibraryA_b:       dq LoadLibraryA_n       
  ReadConsoleInputA_b:  dq ReadConsoleInputA_n  
  ReadFile_b:           dq ReadFile_n           
  SetFilePointer_b:     dq SetFilePointer_n     
  Sleep_b:              dq Sleep_n              
  VirtualAlloc_b:       dq VirtualAlloc_n       
  VirtualFree_b:        dq VirtualFree_n        
  WriteFile_b:          dq WriteFile_n          
 dq 0


dll_name_k:
    db 'KERNEL32.dll', 0

iatbl_size equ $-iatbl

; Import Directory
itbl:
    dq intbl_k                ; OriginalFirstThunk
    dd 0                    ; TimeDateStamp
    dd dll_name_k             ; ForwarderChain
    dd iatbl_k                ; Name

    dq intbl                ; OriginalFirstThunk
    dd 0                    ; TimeDateStamp
    dd dll_name             ; ForwarderChain
    dd iatbl                ; Name


;    dq 0                    ; FirstThunk


;    times 3 dd 0

itbl_size equ $-itbl

; Debug Table
;    times 24 dq 0           ; [UNUSED] Debug Table

%define STD_OUTPUT_HANDLE (-11)

hello_str db "Hello World", 13, 10
hello_size equ ($ - hello_str)

times align($-$$,4)-($-$$) db 0

output_handle dq 0


;    call [rel ExitProcess_a]
QQQ:
    ADD QWORD [rel (MMM + 0x2090)], 1
	MOV RAX, [rel (MMM + 0x2090)]

	MOV  [rel hello_str] , AL

    sub rsp, 48
    mov rcx, [rel output_handle] ;    mov rcx, rax
;    mov rdx,  hello_str+0x400000
    lea rdx,  [rel hello_str] 
    mov r8d,  hello_size
    mov r9, 0
    mov QWORD [rsp + 32], 0
    call [rel WriteFile_b] 
    ;D?truit l'espace de pile allou? pour les param?tres de la fonction
    add rsp, 48
    RET

; Entry
entry:
    ;Aligne la pile
    and rsp, 0xFFFF_FFFF_FFFF_FFF0
    mov rbp, rsp

    mov ecx, STD_OUTPUT_HANDLE
    call [rel GetStdHandle_b] 
    mov [rel output_handle], rax
QQQS:
    CALL QQQ
    JMP QQQS

times align($-$$,4)-($-$$) db 0
MMM: dq 0
;    times align($-$$,16)-($-$$) db 0xcc


sect_size equ $-code

;    times align($-$$,16)-($-$$) db 0

;flat_code_size equ $-code+0x100000
flat_code_size equ 0x200000
code_size equ $-code+SSS_size
file_size equ $-$$+SSS_size
