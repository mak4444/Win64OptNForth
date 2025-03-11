\ Mihail Maksimov, forth@inbox.ru
REQUIRE F7_ED ~mak\Edit.4

REQUIRE <DBG> ~mak\lib\dbg.f 


[IFNDEF] .S  : .S ( -- )    DEPTH .SN ;
[THEN]

MODULE: DBG_MOD

CREATE debugTIB 80 CHARS ALLOT
: eval_debug_statements ( -- )
  \ A simple outer interpreter for interactive input at
  \ debugtime.
   BEGIN
     CR ." D>" debugTIB DUP 80 ACCEPT DUP
   WHILE
     ['] EVALUATE CATCH
     IF ." Oops!?" CR 2DROP
     THEN
   REPEAT
   2DROP ;

0 VALUE DBG_RP@
0 VALUE DBG_Y
CREATE DBG_FILE 200 ALLOT

0 VALUE NNNN

: F7_DO \ ['] DROP TO DBG_DO
 R@
 NEAR_NFA DROP DUP 0= IF
 2DROP BREAK
 NFA2PATH
  DUP 0= IF 2DROP BREAK NIP \ n  path
  RP@ TO DBG_RP@
  NEXT_F
  COUNT
 EDIT_FN!
 EDIT
  >R
  BEGIN BEGIN BEGIN
      CR
      ." S=" .S CR \ KEY? DROP  \ 1000 PAUSE
      R@  EDIT_Y_SET
      FALSE TO ED_WATE
            EdHWnd PUSH-WINDOW DROP
      BEGIN 500 SLEEP ED_WATE UNTIL
      ED_WATE cmdABORT = IF -28 THROW THEN
      ED_WATE cmdHEX   = 
  WHILE  HEX    REPEAT
      ED_WATE cmdDEC   = 
  WHILE DECIMAL  REPEAT
      ED_WATE cmdFORTH = 
  WHILE
\      GUI-CONSOLE::clhwnd PUSH-WINDOW DROP
      eval_debug_statements
  REPEAT 
  RDROP
\  DBG_DO_ TO DBG_DO
;

: F8_DO
  RP@ DBG_RP@  U< 
  IF DROP  EXIT THEN 
  ['] F7_DO >R ;

: F4_DO
  OVER DBG_Y <>
  IF DROP  EXIT THEN
  DUP COUNT 2DUP TYPE  SPACE
 DBG_FILE COUNT  2DUP TYPE
 CEQUAL-U 0=
  IF 2DROP  EXIT THEN
   ['] F7_DO >R
;

: F4_EDIT_
	EDIT_Y_GET TO  DBG_Y
	EDIT_FN COUNT DBG_FILE $!
	['] F4_DO TO DBG_DO ;
: F6_EDIT_ DBG_RP@ CELL+ TO DBG_RP@
		['] F8_DO TO DBG_DO ;
: F7_EDIT_	['] F7_DO TO DBG_DO ;
: F8_EDIT_	['] F8_DO TO DBG_DO ;
: F9_EDIT_	['] DROP TO DBG_DO ;  F9_EDIT_

' F4_EDIT_ TO F4_ED
' F6_EDIT_ TO F6_ED
' F7_EDIT_ TO F7_ED
' F8_EDIT_ TO F8_ED
' F9_EDIT_ TO F9_ED

0 VALUE V_DBG_RP@

EXPORT


: S_DBG_RP@ RP@ TO V_DBG_RP@ ;
: R_DBG_RP@ DBG_RP@ V_DBG_RP@ - RP@ + TO DBG_RP@ ;

0 VALUE MMMM

: DBG_POST DBG_COUNT @ 1 OR
  IF  POSTPONE POSTPONE
  ELSE  MMMM IF  ABORT THEN
         PARSE-NAME 2DROP
  THEN ; IMMEDIATE


DEFER Q-DBG-VECT

' FALSE TO Q-DBG-VECT

: Q-DBG_DO 
   COUNT DBG_FILE $!
   TO DBG_Y 
  ['] DBG_DO >BODY @ >R
  ['] DROP TO DBG_DO
  Q-DBG-VECT
  R> TO DBG_DO
  IF  F7_ED F7_ED
  THEN  
;

: Q-DBG  ( cfa -- )
 TO  Q-DBG-VECT   ['] Q-DBG_DO TO DBG_DO ;

: R>	DBG_POST S_DBG_RP@ POSTPONE R>
	DBG_POST R_DBG_RP@ ; IMMEDIATE

: >R	DBG_POST S_DBG_RP@ POSTPONE >R
	DBG_POST R_DBG_RP@ ; IMMEDIATE

: RDROP	DBG_POST S_DBG_RP@ POSTPONE RDROP
	DBG_POST R_DBG_RP@ ; IMMEDIATE

: {	DBG_POST S_DBG_RP@ POSTPONE {
	DBG_POST R_DBG_RP@ ; IMMEDIATE


\ : : : DBG_COUNT @ IF DBG_<PRE> THEN ;

;MODULE


