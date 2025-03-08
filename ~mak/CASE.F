REQUIRE [IF] ~mak/CompIF3.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f

[IFNDEF] CSP
VARIABLE CSP    \ d3eae0e7e0f2e5ebfc f1f2e5eae0 eaeeedf2f0eeebff
[THEN]

[IFNDEF] ?CSP
: !CSP          ( -- )  \ save current stack pointer for later stack depth check
                SP@ CSP ! ;

: ?CSP          ( -- )  \ check current stack pointer against saved stack pointer
                SP@ CSP @ XOR IF -330 THROW THEN ;
[THEN]


: CASE 
  CSP @ SP@ CSP ! ; IMMEDIATE

: ?OF_ 
  POSTPONE IF POSTPONE DROP ; IMMEDIATE

: OF 
  POSTPONE OVER POSTPONE = POSTPONE ?OF_ ; IMMEDIATE


: ENDOF 
  POSTPONE ELSE ; IMMEDIATE

: DUPENDCASE
  BEGIN SP@ CSP @ <> WHILE POSTPONE THEN REPEAT
  CSP ! ; IMMEDIATE

: END-CASE  POSTPONE DUPENDCASE ; IMMEDIATE

: ENDCASE 
  POSTPONE DROP   POSTPONE DUPENDCASE 
; IMMEDIATE

: OF\
  POSTPONE OVER POSTPONE <> POSTPONE IF ; IMMEDIATE

: OF;
  POSTPONE OVER POSTPONE = POSTPONE IF 2>R
  POSTPONE DUPENDCASE  2R>
  POSTPONE DROP ; IMMEDIATE


