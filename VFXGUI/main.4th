\ from https://www.mpeforth.com/software/pc-systems/vfx-forth-for-windows
.( MAIN.4TH ) CR
REQUIRE END-CASE ~mak\case.f
REQUIRE FILE-SIZE ~mak/lib/ftools.4 
REQUIRE (*	~af\lib\comments.f \ *)
REQUIRE REPLACE-WORD lib/ext/patch.f


[IFNDEF] COMCTL32DLL
DLL_LOAD: Comctl32.DLL COMCTL32DLL
[THEN]

COMCTL32DLL WAPI1: InitCommonControlsEx	InitCommonControlsEx
KERNEL32DLL WAPI1: LoadLibraryA	LoadLibraryA

: /*  ( -- )
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE  S" */" COMPARE 0=  THEN
  UNTIL
; IMMEDIATE

YDP @ 0= [IF] 0xf0000 ALLOCATE THROW DUP  YDP0 ! YDP ! [THEN]
 
\ REQUIRE DBG_INCLUDED lib\include\spf_navigator.f 

\ samples\WIN\EDIT\ED.F
 REQUIRE CASE-INS lib\ext\caseins.f
 REQUIRE { ~mak\locals.f


\- #define : #define  HEADER  CONSTANT-CODE COMPILE,  0 PARSE EVALUATE , ;
\- ?#define : ?#define >IN @  POSTPONE [DEFINED]  IF DROP POSTPONE \ BREAK >IN ! #define  ;


: BINFILE,
   R/O OPEN-FILE THROW >R
   R@  FILE-SIZE THROW D>S
   HERE SWAP R@ READ-FILE THROW
   ALLOT
   R> CLOSE-FILE THROW
;

CREATE ^NULL 0 ,
CREATE CNULL 0 ,

create szSID	\ -- addr ; used as a property string
 S" SID" S, 0 C,
$104 CONSTANT MAX_PATH

CREATE INIFILE$- $100 ALLOT
S" CommandHistory.ini" INIFILE$- $!

\- INCR : INCR 1 SWAP +! ;
\- DECR : DECR -1 SWAP +! ;

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


\- endif : endif POSTPONE THEN ; IMMEDIATE

\- BOUNDS : BOUNDS OVER + SWAP ;

 0
  8 field >liblink		\ link to previous library
  8 field >libaddr		\ library Id/handle/address, depends on O/S
  8 field >libmask		\ mask for dlopen()
  0 field >libname	\ zero terminated string of library name

constant /libstr

\- #HID: : #HID: HEADER  LAST @ NAME>C ! ;

VARIABLE IP-HANDLE
VARIABLE OP-HANDLE

VARIABLE OP-LINE#

?#define MB_ICONWARNING 0x30

 0 constant HWND_DESKTOP

?#define ES_NOHIDESEL	$100
?#define ES_WANTRETURN	$1000
?#define WS_HSCROLL	$100000
?#define WS_VSCROLL	$200000
?#define WS_BORDER	$800000
?#define WS_VISIBLE	$10000000
?#define WS_CHILD	$40000000
?#define CW_USEDEFAULT	$80000000


?#define cmdInclude $444
?#define cmdBye $445
?#define cmdRun $446


0 VALUE hWndRichEdit
 VARIABLE GSID $100 ALLOT

 VARIABLE &hWndMain

-1 VALUE WantStatusbar?

VARIABLE $findmsgstring#

REQUIRE vaultWnd VFXGUI\vault.4 
0 VALUE ScriptRUN?  

VFXGUI/kernel64.fth
VFXGUI\winstrct.fth 
VFXGUI\MessageLoops.fth
VFXGUI\fifo.fth 

VFXGUI\richedit.fth

VFXGUI\TermIDE.fth
VFXGUI\window.fth


: GR-ACCEPT GSID richedit-ACCEPT-
 ScriptRUN?
 IF  DROP 0 TO ScriptRUN?
     S" vault-script.f" INCLUDED
   0
 THEN
 ;
: GR-KEY  GSID richedit-KEY- ;
: GR-KEY? GSID richedit-KEY?- ;
: GR-EMIT GSID richedit-EMIT- ;
: GR-TYPE GSID RICHEDIT-TYPE- ;
: GR-CR GSID richedit-CR-
  (BusyIdle)-
   GSID  richedit-FLUSHOP- drop ;

: WIN-OK  UpdateStatusBar  [ ' OK. DEFER@ COMPILE, ] ;

: WINMAIN-MESS (  -- )
  0 to hWndRichEdit

   CREATEPARENTWINDOW- &hWndMain ! \  

	WantStatusbar?
	if  +STATUSBAR-
	then

	GSID DUP IP-HANDLE ! OP-HANDLE !


	0 $8000 &hWndMain @  GSID RICHEDIT-OPEN- 2drop
	OP-HANDLE @ @  to hWndRichEdit

 INIFILE$- COUNT ['] INCLUDED CATCH IF 2DROP THEN

 &hWndMain @ windowplace SetWindowPlacement- DROP

    GSID @ SETFOCUS DROP

 UpdateStatusBar

;

: TYPE-GUI ( addr u -- )
    H-STDOUT 0 >
    IF   H-STDOUT WRITE-FILE THROW
    ELSE GR-TYPE
    THEN
;

: EMIT-GUI ( c -- )
    H-STDOUT 0 >
    IF   EMIT_M
    ELSE GR-EMIT
    THEN
;

: CR-GUI ( c -- )
    H-STDOUT 0 >
    IF   $D EMIT_M $A EMIT_M
    ELSE GR-CR 
    THEN
;

\- CC_INIT  VARIABLE CC_INIT CC_INIT 0!
\- CC_INITS CREATE   CC_INITS 8 , 0xFFFF ,

[IFNDEF] InitControls

: InitControls
  CC_INIT @ IF EXIT THEN
  CC_INITS InitCommonControlsEx DROP
  Z" RICHED32.DLL" LoadLibraryA DROP
  TRUE CC_INIT !
;

[THEN]

 InitControls

: GUISTART

  0 TO H-STDOUT
  0 TO H-STDERR

  ['] GR-KEY TO KEY
  ['] GR-KEY? TO KEY?
  ['] TYPE-GUI TO TYPE
  ['] EMIT-GUI TO EMIT
  ['] CR-GUI TO CR

  ['] GR-ACCEPT TO ACCEPT
  ['] WIN-OK TO OK.
  WINMAIN-MESS
;

\EOF

 ' InitControls ATCOLD

 ' GUISTART ATCOLD

 COLDCHAIN @ VALUE CHAINSAVE

\ COLDCHAIN 0!
  CC_INIT OFF
 S" WForth64.EXE" SAVE
  CC_INIT ON
