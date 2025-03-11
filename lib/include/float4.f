\ Float-библиотека для spf4
\ Слова высокого уровня
\ [c] Dmitry Yakimov [ftech@tula.net]
\ 64 битная арифметика по умолчанию!

\ + FABORT заменен FNOP 
\ ! исправлен FINIT, автоматическая реинициализация при исключении
\ ! новый необрезающий REPRESENT
\ ! исправлена бесконечность ~yGREK 

\ ! переписан F. ,рефакторинг ( 9.03.2005 ~day )
\ ! пофиксен FS. ( 9.03.2005 ~day )

\ Hi level words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ ~day\float\floatkern.f
USER-CREATE FLOAT-PAD ( -- c-addr ) \ 94 CORE EXT
 \ c-addr - адрес области для формирования строкового представления float 
 \ числа 
0xFF USER-ALLOT

USER FFORM
USER FFORM-EXP
USER ?PRINT-EXP       \ печать с экспонентой или без нее
USER F-SIZE
USER PAST-COMMA \ число знаков после точки
USER ?IS-COMMA  \ появилась точка или нет

USER ?FSTRICT \ дополнять ли точно нулями до PRECISION

: FSTRICT ( -- ? ) ?FSTRICT @ ;
: SET-FSTRICT ( ? -- ) ?FSTRICT ! ;

8 CONSTANT FLOAT

VARIABLE FCON-E

: PRECISION ( -- u )
    FFORM @
;
: SET-PRECISION ( u -- )
   1 MAX 17 MIN FFORM !
;

: PRINT-EXP TRUE ?PRINT-EXP ! ;
: PRINT-FIX FALSE ?PRINT-EXP ! ;

: FSINGLE 4 F-SIZE ! ;
: FDOUBLE 8 F-SIZE ! ;
: FLONG 10  F-SIZE ! ;

: FLOATS F-SIZE @ * ;
: FLOAT+ F-SIZE @ + ;


: FSTATE  \ *
  FLOAT-PAD DUP F>ENV
  8 + W@ 
  ." FPU registers usage:" CR
  8 0 DO
        ." reg " 8 I - . ." :" 
        DUP DUP 1 AND 
        SWAP 2 AND 
        0= IF 1 = IF ."  zero" ELSE ."  valid number" THEN
           ELSE 1 = IF ."  empty" ELSE ."  invalid or infinity" THEN
           THEN
        2 RSHIFT
        CR
      LOOP
  DROP
;
: stackIsEmpty
   ." FPU: Float stack is empty!"
;
HEX

: F, ( F: r -- )
     FDEPTH 0= IF
                 C0000092 THROW
              ELSE
                 HERE FLOAT ALLOT F!   
              THEN
;

CREATE FINF-ADDR 0 L, 80000000 L, 7FFF L, \ Infinity

DECIMAL

: FINF FINF-ADDR F@ ;
: -FINF FINF FNEGATE ;

: TNUM ( addr u -- d )       \ *
   0. 2SWAP >NUMBER 2DROP
;

: F10X ( u -- R: 10^u )
   1.E
   DUP 0 > IF    0 DO F10* LOOP
           ELSE  NEGATE 0 ?DO F10/ LOOP
           THEN
;

: SEARCH-EXP ( c-addr1 u -- c-addr2 u flag )
    BEGIN
      OVER C@ -1
      OVER [CHAR] e <> AND
      OVER [CHAR] E <> AND
      OVER [CHAR] d <> AND
      SWAP [CHAR] D <> AND
      OVER AND
    WHILE
      1- SWAP 1+ SWAP
    REPEAT
;

: GET-EXP ( addr u -- d )    \ *
  SEARCH-EXP DUP
  IF                    \ addr u
    >R 1+ DUP C@        \ addr1 c
    DUP [CHAR] - =
    IF DROP 1+ R> 2 - TNUM DNEGATE   
    ELSE [CHAR] + = IF 1+ R> 2 - TNUM ELSE R> 1- TNUM THEN
    THEN
  ELSE
    2DROP 0.
  THEN
;


: FRAC>F ( addr u -- F: r )              \ 
  .E
  OVER + SWAP
  DO
    ?IS-COMMA @ IF PAST-COMMA 1+! THEN
    I C@ DUP 47 > 
    OVER 58 < AND
    IF 48 - S>F F+ F10* 
    ELSE [CHAR] . = IF TRUE ?IS-COMMA ! 
                         ELSE LEAVE
                         THEN
    THEN
  LOOP
;


: FRAC>F ( addr u -- F: r )              \ 
  .E
  OVER + SWAP
  DO
    ?IS-COMMA @ IF PAST-COMMA 1+! THEN
    I C@ DUP 47 > 
    OVER 58 < AND
    IF 48 - S>F F+ F10* 
    ELSE [CHAR] . = IF TRUE ?IS-COMMA ! 
                         ELSE LEAVE
                         THEN
    THEN
  LOOP
;

: ?IE GETFPUSW 1 AND 0<> ;
: ?ZE GETFPUSW 4 AND 0<> ;
: ?OF GETFPUSW 8 AND 0<> ;

: >FLOAT-ABS  ( addr u -- F: r D:  bool )
   BASE @ >R DECIMAL
   GETFPUCW >R UP-MODE 
   2DUP GET-EXP DROP         \  addr u u2 - 
   ROT ROT FRAC>F             \ u2
   ?IS-COMMA @ 0= IF PAST-COMMA 1+! THEN
   PAST-COMMA @ - F10X  F*  ?OF ?IE OR 
   DUP IF FDROP THEN INVERT
   R> SETFPUCW
   R> BASE !
;

\- SKIP1 : SKIP1 ( addr u -- addr+1 u-1 ) DUP 0 > IF 1 - SWAP CHAR+ SWAP THEN ;

: CHECK-SET ( addr u max min addr2 u2 -- addr2 u2 bool )
    >R >R >R OVER MIN >R SWAP R>
    0 >R \ D: addr u1 R: u2 addr2 min 0
    BEGIN
      DUP R@ >
    WHILE
      OVER R@ + C@
      2 CELLS RP+@
      3 CELLS RP+@       
      ROT 
      >R RP@ 1 SEARCH RDROP NIP NIP
      0= IF     \ 
           DROP SWAP
           R@ - SWAP R@ + SWAP 
           2R> 1+ < RDROP RDROP EXIT
         THEN
      R> 1+ >R
    REPEAT
    + SWAP R@ -
    2R> 1+ < RDROP RDROP
;

: <SIGN> ( addr u max min -- addr2 u2 bool )
    S" -+" CHECK-SET
;

: <EXP> ( addr u max min -- addr2 u2 bool )
    S" EeDd" CHECK-SET
;

: <DOT> ( addr u max min -- addr2 u2 bool )
    S" ." CHECK-SET
;

: <DIGITS> ( addr u max min -- addr2 u2 bool )
    S" 0123456789" CHECK-SET
;

: ?FLOAT ( addr u -- bool )
    1   0 <SIGN>    >R
    16  0 <DIGITS>  >R
    1   0 <DOT>     >R
    16  0 <DIGITS>  >R
    1   1 <EXP>     >R
    1   0 <SIGN>    >R
    4   0 <DIGITS>  >R
    NIP 0= \ 
    2R> 2R> 2R> R> AND
    AND AND AND AND AND
    AND
;

: >FLOAT ( addr u -- F: r true | false )
  2DUP ?FLOAT
  IF
    PAST-COMMA 0! FALSE ?IS-COMMA !
    OVER C@ DUP [CHAR] - =    \ addr u c flag
    IF DROP SKIP1 >FLOAT-ABS FNEGATE
    ELSE [CHAR] + = IF SKIP1 THEN
                    >FLOAT-ABS
    THEN
  ELSE
   2DROP 0
  THEN
;


HEX

: FABORT
      ?IE IF FINIT C0000090  THROW THEN    \ invalid operation
      ?OF IF FINIT C0000091 THROW THEN     \ overflow
      ?ZE IF FINIT C000008E THROW THEN     \ divide by zero
;
DECIMAL

\ Младшие шесть бит маскируют ошибки #I #D #Z #O #U #P
: ERROR-MODE \ Включает ошибки сопроцессора кроме #P 
             \ потому что #P реагирует на ноль
   GETFPUCW
   127 INVERT AND
   32 OR
   SETFPUCW
;
: NORMAL-MODE \ Всё тихо крома #I и стека
  GETFPUCW
  127 OR
  1 INVERT AND
  SETFPUCW
;

: SILENT-MODE \ Всё тихо
  GETFPUCW
  127 OR
  SETFPUCW
;

: F10X ( u -- R: 10^u )
   1.E
   DUP 0 > IF    0 DO F10* LOOP
           ELSE  NEGATE 0 ?DO F10/ LOOP
           THEN
;


DECIMAL


: FLOOR ( F: r1 -- r2 )
    GETFPUCW >R
    LOW-MODE
    FINT
    R> SETFPUCW
;
: FROUND ( F: r1 -- r2 )
    GETFPUCW >R
    ROUND-MODE
    FINT
    R> SETFPUCW
;

\ Дает число знаков целой части числа

: #EXP ( -- n ) ( r -- r )  FDUP F0=  IF PRECISION  ELSE
   FDUP FABS FLOG FLOOR FR>S  THEN
;

: F>D FR>S S>D ;
: F>DS FR>S ;
: D>F DROP S>F ;


: 0.1E
   1.E F10/
;

16 VALUE MPREC  \ your maximum precision
CREATE EXP 0 , 0 ,  \ exponent & sign

\ from http://www.alphalink.com.au/~edsa/represent.html
: REPRESENT  ( c-addr u -- n flag1 flag2 ) ( F: r -- )
     2DUP [CHAR] 0 FILL
     MPREC MIN  2>R
     FDUP F0< 0 EXP 2!
     FABS  FDUP F0= 0=
     BEGIN  WHILE
	CR ." REP22=" FDUP F>D H.  H. 
       FDUP 1.E F< 0= IF
         F10/
         1
       ELSE
         FDUP 0.1E F< IF
           F10*
           -1
         ELSE
           0
         THEN
       THEN
       DUP EXP +!
     REPEAT
     1.E  R@ 0 ?DO F10* LOOP  F*
     FROUND F>D
     2DUP <# #S #>  DUP R@ - EXP +!
     2R>  ROT MIN 1 MAX CMOVE
     D0=  EXP 2@ SWAP  ROT IF 2DROP 1 0 THEN  \ 0.0E fix-up
     TRUE 
;
\- DEFER : DEFER VECT ;
DEFER FTYPE
DEFER FEMIT

\ trim trailing '0's
: (T0)  ( c-addr u1 -- c-addr u2 )
  FSTRICT IF EXIT THEN
  BEGIN  DUP WHILE  1- 2DUP CHARS +
  C@ [CHAR] 0 -  UNTIL  1+  THEN ;

: FDISPLAY ( n -- )
   DUP 1 <
   IF \ число < 1      
      DUP 1 < IF [CHAR] 0 FEMIT THEN
      [CHAR] . FEMIT
      ABS DUP 0 ?DO [CHAR] 0 FEMIT LOOP
      FLOAT-PAD PRECISION ROT - 1- 0 MAX
      (T0) FTYPE
   ELSE
     FLOAT-PAD OVER 0 MAX PRECISION MIN FTYPE
     \ выведем дробную часть
     [CHAR] . FEMIT
     DUP FLOAT-PAD + PRECISION ROT - 0 MAX
     (T0)
     \ DUP 0= IF [CHAR] 0 FEMIT THEN
     FTYPE
     THEN   
;

: format-exp ( ud1 -- ud2 ) \ *
  UP-MODE
  2DUP 2DUP D0= IF 2DROP 0 ELSE D>F F[LOG] F>DS THEN
  FFORM-EXP @ MAX
  0
  DO # LOOP
;

: .EXP
     BASE @ >R DECIMAL
     S>D
     DUP >R DABS <# format-exp R> SIGN FCON-E @ HOLD #>
     FTYPE
     R> BASE !
;

: PrintFInf ( F: r -- r ) ( -- -1 )
            ( F: r -- ) ( -- 0 )
     FDUP  FINF F= IF  S" infinity " FTYPE FDROP TRUE EXIT THEN
     FDUP -FINF F= IF S" -infinity " FTYPE FDROP TRUE EXIT THEN
     FALSE
;

: (F.) ( n1 n2 )
\ n1 - exponent
\ n2 - sign
   FLOAT-PAD PRECISION REPRESENT DROP
;

: FS. ( r -- )
   PrintFInf IF EXIT THEN
   (F.) IF [CHAR] - FEMIT THEN
   1 FDISPLAY 1- .EXP SPACE
;

: F. ( r -- )
   PrintFInf IF EXIT THEN
   (F.) IF  [CHAR] - FEMIT THEN
   FDISPLAY SPACE
;

: G. ( r)
   PrintFInf IF EXIT THEN
   (F.) IF  [CHAR] - FEMIT THEN
   DUP ABS PRECISION > 
   IF 
      1 FDISPLAY 1- .EXP
   ELSE FDISPLAY
   THEN
   SPACE
;
   
: Adjust ( n - n' 1|2|3 )
   S>D 3 FM/MOD 3 * SWAP 1+  ;

: FE. ( r)
   PrintFInf IF EXIT THEN
   (F.) IF  [CHAR] - FEMIT  THEN
   1- Adjust FDISPLAY .EXP SPACE
;

USER PAD-COUNT

: C-TO-PAD ( c )
   PAD-COUNT @ PAD + C!
   PAD-COUNT 1+!
   PAD-COUNT @ 1024 1- >
   ABORT" Too big float number, buffer overflow"
;

: S-TO-PAD ( addr u )
   OVER + SWAP
   ?DO
      I C@ C-TO-PAD
   LOOP
;

: >FNUM ( F: r  -- addr u )
   PAD-COUNT 0!
   ['] C-TO-PAD TO FEMIT
   ['] S-TO-PAD TO FTYPE
   ?PRINT-EXP @
   IF
     FS.
   ELSE
     G.
   THEN
   ['] EMIT TO FEMIT
   ['] TYPE TO FTYPE
   PAD PAD-COUNT @
;


\ DB 2D - TBYTE
\ DD 05 - QWORD
\ D9 05 - DWORD

: DFLOAT+ ( addr1 -- addr2 )
    8 +
;
: DFLOATS ( n1 -- n2 )
    3 LSHIFT
;
: SFLOAT+ ( addr1 -- addr2 )
    CELL+
;
: SFLOATS ( n1 -- n2 )
    2 LSHIFT
;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

(
: FHYPTMP \ *
   F1+ FLOG2 FLN2 F*
;

)
: HIGH-FINIT
     8 SET-PRECISION
     TRUE SET-FSTRICT
     2 FFORM-EXP !
     FDOUBLE
     FINIT
     PRINT-EXP
     NORMAL-MODE
     [CHAR] e FCON-E !
     ['] EMIT TO FEMIT
     ['] TYPE TO FTYPE
;


HIGH-FINIT
