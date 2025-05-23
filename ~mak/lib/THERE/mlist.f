REQUIRE PLACE  ~mak\place.f 
REQUIRE [IF] ~mak/CompIF3.f
[IFNDEF] BREAK : BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE  [THEN]

C" STREAM-FILE" FIND NIP
[IF]
: FROM_SOURCE-ID SOURCE-ID  STREAM-FILE ;
: TO_SOURCE-ID FILE>RSTREAM TO SOURCE-ID ;
[ELSE]
: FROM_SOURCE-ID SOURCE-ID ;
: TO_SOURCE-ID TO SOURCE-ID ;
[THEN]

C" -CELL" FIND NIP 0=
[IF] -1 CELLS CONSTANT -CELL
[THEN]

[IFNDEF] HERE-TAB-CUR VARIABLE HERE-TAB-CUR
[THEN]

[IFNDEF] SHERE-TAB-CUR VARIABLE SHERE-TAB-CUR
[THEN]
\- M+! : M+! +! ;

\- C/L $FF CONSTANT C/L

MODULE: AVRLIST

CREATE  FILE_NAME_L 120 ALLOT

CREATE   HERE-TAB  4000 CELLS ALLOT
HERE CELL-  CONSTANT HERE-TAB-MAX
HERE-TAB HERE-TAB-CUR !
VARIABLE S_STATE 

: HERE-TAB-CUR+
  HERE-TAB-CUR @  CELL+ HERE-TAB-MAX UMIN
  HERE-TAB-CUR
 !
\ [ .( XXXX) DIS-OPT KEY DROP ]
 ;

0 VALUE CURFILEHTC

: HERE-TO-TAB-FORCE
 DP M@
 HERE-TAB-CUR M@
\ ." |" dup h.
 M!
 HERE-TAB-CUR+
 ;

: HERE-TO-TAB \ F7_ED
\+ CURFILE CURFILEHTC 0= IF CURFILE @ TO CURFILEHTC THEN
\+ CURFILE CURFILE @ CURFILEHTC <> IF BREAK
 HERE-TO-TAB-FORCE
 ;


CREATE   SHERE-TAB  700 CELLS ALLOT
HERE CELL-  CONSTANT SHERE-TAB-MAX
SHERE-TAB SHERE-TAB-CUR !

: SHERE-TAB-CUR+
  SHERE-TAB-CUR M@  CELL+ SHERE-TAB-MAX UMIN
  SHERE-TAB-CUR M! ;

: SHERE-TO-TAB DP M@ SHERE-TAB-CUR M@ M! SHERE-TAB-CUR+ ;

80 VALUE DUMP_MAX 


: .LIST ( ADDR  ADDR1 -- ADDR1' ) 
\          CR H. EXIT
          S_STATE @ DROP 1
          IF
             SWAP
             BEGIN  2DUP U> 
             WHILE  MINST
             REPEAT  NIP CR
          ELSE
            TUCK   
            OVER - DUP
            IF   DUP DUMP_MAX U>

                IF  >R DUMP_MAX DUMP 
                    CR DUP U.  R> DUMP_MAX - U. ." bytes"
                ELSE  DUMP
                THEN CR
            ELSE 2DROP
            THEN
          THEN
;

VECT INCLUDED$

' INCLUDED TO INCLUDED$

EXPORT

: CON_EMIT
   H-STDOUT >R  [ H-STDOUT LIT, ] TO H-STDOUT EMIT
   R> TO H-STDOUT ;

: STCR_
 ." //"    SOURCE TYPE CR
 ; 

VECT STCR
' STCR_ TO STCR

HERE S" _AL" S", VALUE C"_AL"

0 VALUE INCL-HH

: INCLUDED_AL
 HERE TO INCL-HH
\ F7_ED
   ['] <PRE> >BODY @ >R
   ['] HERE-TO-TAB TO <PRE>
	0 TO CURFILEHTC
     HERE-TAB  HERE-TAB-CUR !
    SHERE-TAB SHERE-TAB-CUR ! 
  2DUP 2>R
  INCLUDED$ 
 2R>  R> TO <PRE>
 -1 SHERE-TAB-CUR M@ M!  SHERE-TAB-CUR+
    HERE-TO-TAB-FORCE
    HERE-TO-TAB-FORCE	-CELL HERE-TAB-CUR M+!
    HERE-TAB-CUR M@ M@	-CELL HERE-TAB-CUR M+!
    BEGIN HERE-TAB-CUR M@ HERE-TAB <>
    WHILE HERE-TAB-CUR M@ M@ UMIN DUP HERE-TAB-CUR M@ M!
          -CELL HERE-TAB-CUR M+!
    REPEAT DROP
    S_STATE 0!
    SHERE-TAB SHERE-TAB-CUR M! 

    2DUP 1-  FILE_NAME_L  PLACE
  C"_AL" COUNT FILE_NAME_L +PLACE  
\  S" _AL"  FILE_NAME_L +PLACE  
\ CR ."  R/O OPEN-FILE=" 2DUP TYPE KEY DROP
  R/O OPEN-FILE  THROW
  FILE_NAME_L COUNT 2DUP + 0!
\   2DUP TYPE
  W/O CREATE-FILE THROW

  TIB >R >IN M@ >R #TIB M@ >R SOURCE-ID >R ( BLK M@ >R) CURSTR M@ >R
  H-STDOUT >R  BASE M@ >R HEX
  C/L 2 + ALLOCATE THROW TO TIB \ BLK 0!

\ DROP \
 TO H-STDOUT
\+ NOTIV? -1 TO NOTIV?
  TO_SOURCE-ID
  CURSTR 0! HERE-TAB-CUR @ @
  BEGIN    REFILL
  WHILE \ [CHAR] * CON_EMIT
        STCR
        BEGIN  SHERE-TAB-CUR M@ M@ HERE-TAB-CUR M@ CELL+ M@ U<
        WHILE  SHERE-TAB-CUR M@ M@ .LIST   SHERE-TAB-CUR+
                 S_STATE @ INVERT S_STATE !
        REPEAT  HERE-TAB-CUR+ HERE-TAB-CUR @ @ .LIST
  REPEAT  DROP
  TIB FREE THROW
  FROM_SOURCE-ID ?DUP IF CLOSE-FILE THROW THEN
   H-STDOUT 1 <> IF H-STDOUT CLOSE-FILE THROW THEN
  R> BASE M! R> TO H-STDOUT
  R> CURSTR M! ( R> BLK M!) R> TO SOURCE-ID R> #TIB M! R> >IN M! R> TO TIB
 CR FILE_NAME_L COUNT TYPE 9 EMIT HERE INCL-HH - .

;

: REQUIRED_AL ( waddr wu laddr lu -- )
  2SWAP SFIND
  IF DROP 2DROP EXIT
  ELSE 2DROP INCLUDED_AL THEN
;
: REQUIRE_AL ( "word" "libpath" -- )
  BL PSKIP BL PARSE
  BL PSKIP BL PARSE 2DUP + 0 SWAP C!
  REQUIRED_AL
;

: FLOAD_AL  BL PSKIP BL PARSE INCLUDED_AL ;

;MODULE

\EOF
: : : SHERE-TO-TAB ;

: ; POSTPONE ; SHERE-TO-TAB ; IMMEDIATE

: SSSS
     HERE-TAB  HERE-TAB-CUR !
    SHERE-TAB SHERE-TAB-CUR ! 
;
