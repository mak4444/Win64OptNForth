

REQUIRE <DBG> ~mak\lib\DBGWC.F

<DBG> 

 ~mak/amd64/hasmx64.fth

\ REQUIRE DISP ~mak/amd64/disasm.fs
REQUIRE DISA ~mak\amd64\hdasmx64.fth 


\ REQUIRE DISP ~mak/amd64/disasm.fs
REQUIRE DISA ~mak\amd64\hdasmx64.fth 

: WWW1   [  $9A C, $40 C, 0 C, 0 W, 0 W,  ]  ;

.( F7 - step into ) CR

7 9

F7_ED SEE WWW1 
