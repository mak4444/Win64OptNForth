\+ DBG_CURFILE REQUIRE DBG_INCLUDED lib\include\spf_navigator.f 
\+ VIEW_LINK  REQUIRE VIEWS_SEARCH ~mak/tools/DEFVIEW.F
REQUIRE SYSCALL0 ~mak\lib\syscall.f
CREATE C_EDITBUFF 256 ALLOT

\- +NULL : +NULL COUNT + 0 SWAP C!  ;

CREATE N++PATH 200 ALLOT

 S" notepad++" N++PATH  $!

: $EDIT ( <filename> m -- )
 
 N++PATH COUNT C_EDITBUFF $!
S"  -lnormal -n" C_EDITBUFF $+!
 0 (D.)  C_EDITBUFF $+!
  BL  C_EDITBUFF C+PLACE \ $C+!
 C_EDITBUFF $+! 
  C_EDITBUFF +NULL
 C_EDITBUFF  COUNT SYSCALL0 DROP \ ." <" H. ." >" \  ." <" TYPE ." >" 
;
 
: N++ ' NEAR_NFA DROP ?DUP 
  IF 9 - DUP @ COUNT 2>R CELL- @  2R> ROT
  $EDIT 
  THEN
;

CREATE CUR-NFILEBUFF 256 ALLOT

: $CUR-NFILE  CUR-NFILEBUFF $! ;

: CUR-NFILE  PARSE-NAME $CUR-NFILE ;

: N++ED  CUR-NFILEBUFF COUNT 0  $EDIT  ;

: NLOED  CUR-NFILEBUFF COUNT INCLUDED ;

1 [IF]

[IFNDEF] ERROR
\- ERRFILENAME : ERRFILENAME ERRFILE ;
\- ERR-LINE : ERR-LINE ERCURSTR ;

: EDIT_ERROR ( ERR-NUM -> ) \ 
  [ ' ERROR_DO >BODY @ COMPILE, ]
  ERRFILENAME COUNT NIP \ cmdDBG ED_WATE <> AND
  IF  ERRFILENAME COUNT ERR-LINE @ $EDIT
  THEN
;

' EDIT_ERROR TO ERROR_DO

: EE>   PARSE-NAME VIEWS_SEARCH ?DUP
  IF COUNT  ROT NAME>F 4 + L@  $EDIT  THEN  
;

[ELSE]
: N++_ERROR ( ERR-NUM -> ) \ 
  [ ' ERROR >BODY @ COMPILE, ]
  ERRFILE COUNT NIP \ cmdDBG ED_WATE <> AND
  IF	ERRFILE COUNT $CUR-NFILE
	ERRFILE COUNT ERCURSTR @  $EDIT
  THEN
;

' N++_ERROR TO ERROR

: E>
 ' NEAR_NFA DROP ?DUP 
  IF 
   DUP  8 4 * - @ ?DUP
	IF 	COUNT $CUR-NFILE 12 - L@ CUR-NFILEBUFF COUNT ROT	$EDIT
	THEN
  THEN
;

[THEN]
[THEN]

0 [IF]


: N++_ERROR ( ERR-NUM -> ) \ 
  [ ' ERROR >BODY @ COMPILE, ]
  ERRFILE COUNT NIP \ cmdDBG ED_WATE <> AND
  IF	ERRFILE COUNT $CUR-NFILE
	ERRFILE COUNT ERCURSTR @  $EDIT
  THEN
;

' N++_ERROR TO ERROR

: E>
 ' NEAR_NFA DROP ?DUP 
  IF 
   DUP  8 4 * - @ ?DUP
	IF 	COUNT $CUR-NFILE 12 - L@ CUR-NFILEBUFF COUNT ROT	$EDIT
	THEN
  THEN
;

[THEN]

\ S" D:\masm32\asmspf\src\macroopt.f" 111 $EDIT 
\ S" notepad++" SYSCALL0
\ S" notepad++ -lnormal -n77 D:\masm32\asmspf\src\macroopt.f" SYSCALL0
