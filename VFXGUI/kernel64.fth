[IFDEF] ZCOUNT
: ZCOUNT        \ zaddr -- zaddr len
\ *G A version of *\fo{COUNT} for zero terminated strings, returning
\ ** the address of the first character and the length.
  dup
  begin
    dup c@ ( 0<> )
   while
     1+
  repeat
  over -
;
[THEN]

: zterm		\ caddr len -- caddr len
\ *G Zero terminate the given string.
  2dup + 0 swap c!
;

: zplace        \ caddr len zaddr --
\ *G Copy the string caddr/len to zaddr as a 0 terminated string.
  2dup + >r				\ save location of zero
  swap move				\ copy string
  0 r> c!				\ terminate afterwards in case of overlap
;

: zmove		\ src dst -- ; shows off the optimiser
\ *G Copy a zero terminated string.
  begin
    over c@ 2dup
    swap c!
    rot 1+ rot 1+ rot
    0=
  until
  2drop
;

: zAppend	\ caddr len zdest --
\ *G Add the string defined by *\i{caddr/len} to the end of the
\ ** zero terminated string at *\i{zdest}.
  zcount + zplace
;


CREATE ZNULL 0 ,

\- 4dup : 4dup ( d1 d2 -- d1 d2	d1 d2 ) 2over 2over ;

\- 4drop : 4drop ( d1 d2 -- ) 2drop 2drop ;

\- CELLS+ : CELLS+ CELLS + ;

\- AND! : AND! DUP @ ROT AND SWAP !  ;

\- BIC! : BIC! swap invert swap and! ;
\- <=  : <=  > 0= ;

\- ANDC : ANDC INVERT AND ;

: HIWORD        \ n -- n16
( *G Mask off the high 16 bits of a cell and shift right by 16 bits.   )
  $10 rshift  $FFFF and
;

: LOWORD        \ n -- n16
( *G Mask off the low 16 bits of a cell.                               )
  $FFFF and
;

