REQUIRE [IF] ~mak/CompIF3.f

C" [DEFINED]" FIND NIP 0=
[IF]
: [DEFINED] ( -- f ) \ "name"
  NextWord  SFIND  IF DROP TRUE ELSE 2DROP FALSE THEN
; IMMEDIATE
[THEN]

C" [UNDEFINED]" FIND NIP 0=
[IF]
: [UNDEFINED]  ( -- f ) \ "name"
  POSTPONE [DEFINED] 0=
; IMMEDIATE
[THEN]

: [IFDEF]
    POSTPONE [DEFINED]
    0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [IFNDEF]
    POSTPONE [UNDEFINED]
    0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

