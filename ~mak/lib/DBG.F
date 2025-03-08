\ Mihail Maksimov

\-  DBG_DO ' DROP ->DEFER DBG_DO

' DBG_DO VALUE 'DBG_DO

\ MODULE: DBG_MOD

DEFER OLD_<PRE>

: DBG_<PRE>
  STATE @
  IF  CURSTR @ 1- 0 MAX LIT,
      'DBG_DO COMPILE,  OLD_<PRE>
  THEN
;

\ EXPORT

VARIABLE DBG_COUNT  DBG_COUNT 0!

: ?NO_DBG  DBG_COUNT @ ABORT" DBG" ; IMMEDIATE

: DBG_START
  ['] <PRE> >BODY @ TO OLD_<PRE>
  [']  DBG_<PRE> TO <PRE> 
  1 DBG_COUNT !
;

: DBG_STOP
  ['] OLD_<PRE> >BODY @ TO <PRE>
  DBG_COUNT 0!
;
: <DBG>
  DBG_COUNT @ 0=
  IF DBG_START EXIT THEN
  DBG_COUNT  1+!
;         IMMEDIATE

: <\DBG>
  -1 DBG_COUNT  +!
  DBG_COUNT @ 0=
  IF DBG_STOP THEN
;         IMMEDIATE

\ ;MODULE

