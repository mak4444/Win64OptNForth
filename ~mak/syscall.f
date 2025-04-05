
REQUIRE WAPI10: lib\WAPI.4 
KERNEL32DLL WAPI10: CreateProcessA CreateProcessA
\ WINAPI: WaitForSingleObject KERNEL32.DLL

0
4 FIELD cb
4 FIELD lpReserved
4 FIELD lpDesktop
4 FIELD lpTitle
4 FIELD dwX
4 FIELD dwY
4 FIELD dwXSize
4 FIELD dwYSize
4 FIELD dwXCountChars
4 FIELD dwYCountChars
4 FIELD dwFillAttribute
4 FIELD dwFlags
2 FIELD wShowWindow
2 FIELD cbReserved2
4 FIELD lpReserved2
4 FIELD hStdInput
4 FIELD hStdOutput
4 FIELD hStdError
100 +
CONSTANT /STARTUPINFO

HEX 00000100 CONSTANT STARTF_USESTDHANDLES DECIMAL
CREATE STARTUPINFO /STARTUPINFO ALLOT STARTUPINFO /STARTUPINFO ERASE

/STARTUPINFO STARTUPINFO cb L!
STARTF_USESTDHANDLES STARTUPINFO dwFlags L!
H-STDIN STARTUPINFO hStdInput L!
H-STDOUT STARTUPINFO hStdOutput L!
H-STDERR STARTUPINFO hStdError L!

: SYSCALL0 ( S" application.exe" -- flag )
  OVER + 0 SWAP C! >R

  0    \ application
  R>   \ command line
  0 0  \ process & thread security
  FALSE \ inherit handles
  0    \ creation flags
  0    \ environment
  0    \ current dir
  STARTUPINFO \ startup info
  PAD \ process information
  CreateProcessA

 \ DUP	  IF -1 HERE @ WaitForSingleObject DROP THEN
;


\ EOF


