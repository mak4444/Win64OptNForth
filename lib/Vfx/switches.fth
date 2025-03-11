\ SWITCHES for VFX Forth

(*
Documentation
=============
Derived from an article in Forth Dimensions vol. XX no. 3
by Rick vanNorman.

The SWITCH structure is built as follows:
HEAD
cell	link to previous switch
cell	anchor for item chain
cell	default action

ITEM
cell	link to previous item
cell	id for this item
cell	xt to run
*)

\ ========
\ *> tools
\ *S Switch chains
\ ========
\ *N Introduction
\ *P Switch chains provide a mechanism for generating extensible chains similar
\ ** to the CASE ... OF ... ENDOF ... ENDCASE control structure, except that
\ ** the user may extend the chain at any time. These chains are of particular use
\ ** when defining winprocs whose action may need to be adjusted or extended after
\ ** the chain itself has been defined.

\ *P The following example shows how to define a simple chain that
\ ** translates numbers to text. At a later date, translations in
\ ** Italian are added.

\ *P Define some words which will be executed by the chain.
\ *E : one   ." one"  ;
\ ** : two   ." two"  ;
\ ** : three ." three" ;
\ ** : many  . ." more" ;

\ *P The following definition defines a switch called NUMBERS which
\ ** executes ONE when 1 is the selector, TWO if 2 is the selector,
\ ** or MANY if any other number is the selector. Note that MANY must
\ ** consume the selector. The word RUNS associates a word with the
\ ** given selector.
\ *E [switch numbers many
\ **   1 runs one
\ **   2 runs two
\ ** switch]
\ ** cr 1 numbers
\ ** cr 5 numbers

\ *P The next piece of code extends the NUMBERS switch chain, and
\ ** demonstrates the use of RUN: to define an action without giving
\ ** it a name.
\ *E [+switch numbers
\ **   3 runs three
\ **   4 run: ." four" ;
\ **   5 run: ." five" ;
\ ** switch]
\ ** cr 1 numbers
\ ** cr 5 numbers
\ ** cr 8 numbers

\ *P The following portion of this example demonstrates how selectors
\ ** are overridden by the last action defined. Although an action
\ ** has already been defined for selectors 1 and 2, if another action
\ ** is defined, it will be found before the old ones, and so the action
\ ** will be performed.
\ *E [+switch numbers
\ **   1 run: ." uno" ;
\ **   2 run: ." due" ;
\ ** switch]
\ ** cr 1 numbers
\ ** cr 2 numbers
\ ** cr 3 numbers
\ ** cr 5 numbers
\ ** cr 8 numbers

\ ====================
\ *N Switches glossary
\ ====================

variable switch-link	\ -- addr ; links all switches

0
  cell field sw_link		\ link to previous switch
  cell field sw_itemlink	\ anchor for item chain
  cell field sw_defxt	        \ default action
DROP

0
  cell field itm_link	        \ link to previous item
  cell field itm_id	        \ id for this item
  cell field itm_xt	        \ xt to run
DROP

: switch	\ i*x id switchhead -- j*x
\ *G Given an id and the head of a switch chain, *\fo{SWITCH}
\ ** will perform the action of the given id if it is found,
\ ** otherwise it will perform the default action, passing id
\ ** to that action.
  dup sw_defxt @ >r		\ save default action
  sw_itemlink @
  begin				\ -- id item
\ CR ." sw33=" .S CR
    ?dup
   while			\ -- id item
\ cr over .xword space  dup itm_id @ .xword
    2dup itm_id @
\ CR ." sw43=" .S CR
 =		\ check ids match
    if
      nip itm_xt @ execute
      r> drop  exit
    THEN
    itm_link @
  repeat
  r> execute
;
[IFNDEF] link,
: link,		\ addr -- ; update chain anchored at addr
  here  over @ ,  swap !
;
[THEN]

: SwitchHead,	\ -- addr ; builds a switch chain head
  here				\ address of head
  switch-link link,		\ links all switches
  0 ,				\ no items added yet
  ' ,				\ set default action
;

: ItemHead,	\ head id -- head item
  here >r			\ address of item head
  over sw_itemlink link,	\ link to previous item
  ,				\ id
  0 ,				\ dummy xt
  r>
;

: [switch	\ "default" -- head ; i*x id -- j*x
  create
    ( align ) SwitchHead,
  does>
    ( aligned ) switch
;
: [+switch	\ "head" -- head ; to extend an existing switch
  ' >body
;

\- LAST-NON 0 VALUE LAST-NON
\- :NONAME : :NONAME ( C: -- colon-sys ) ( S: -- xt ) \ 94 CORE EXT
\- :NONAME  HERE DUP TO LAST-NON [COMPILE] ] ;

: run:		\ head id -- head ; add nameless action to switch
  itemhead,  :noname swap itm_xt !  !csp
;

: runs		\ head id "word" -- head
  itemhead,  ' swap itm_xt !
;

: switch]	\ head -- ; finishes a switch chain or extension
  drop
;

: inSwitch?	\ id xt -- flag
\ *G Returns true if the *\b{id} is in the switch chain given by
\ ** its *\b{xt}.
  >body sw_itemlink @
  begin
    dup
   while				\ -- id item
    2dup itm_id @ <>			\ exit on match
   while
    itm_link @
  repeat then
  nip  0<>
;


\ *********
\ test code
\ *********

0 [if]

: one   ." one"  ;
: two   ." two"  ;
: three ." three" ;

: many  . ." more" ;

[switch numbers many
  1 runs one
  2 runs two
switch]

((
cr 1 numbers
cr 5 numbers
))

[+switch numbers
  3 runs three
  4 run: ." four" ;
  5 run: ." five" ;
switch]

((
cr 1 numbers
cr 5 numbers
cr 8 numbers
))

[+switch numbers
  1 run: ." uno" ;
  2 run: ." due" ;
switch]

((
cr 1 numbers
cr 2 numbers
cr 3 numbers
cr 5 numbers
cr 8 numbers
))

[then]


\ ======
\ *> ###
\ ======
