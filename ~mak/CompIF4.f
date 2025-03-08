
\- V_UPPER DEFER V_UPPER

' 2DROP TO V_UPPER

: [ELSE]
    1
    BEGIN
      PARSE-NAME 2DUP V_UPPER DUP
      IF  
         2DUP   S" \"  COMPARE 0=   IF 2DROP POSTPONE \	ELSE 
         2DUP 3 UMIN  S" [IF"  \ a2e1a5 ᫮a2a0 e1 afe0a5䨪ᮬ "[IF"
                        COMPARE 0= IF 2DROP 1+                 ELSE 
         2DUP S" [ELSE]" COMPARE 0= IF 2DROP 1- DUP  IF 1+ THEN ELSE 
              S" [THEN]" COMPARE 0= IF       1-                 THEN
                                    THEN THEN THEN
      ELSE 2DROP REFILL  AND \   SOURCE TYPE
      THEN DUP 0=
    UNTIL  DROP ;  IMMEDIATE

: [IF] 0= IF POSTPONE [ELSE] THEN ;  IMMEDIATE

: [THEN] ;  IMMEDIATE


