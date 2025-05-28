DECIMAL

[IFNDEF] GDI32DLL
DLL_LOAD: Gdi32.DLL GDI32DLL
[THEN]

USER32DLL WAPI1: RegisterWindowMessageA	RegisterWindowMessageA
USER32DLL WAPI12: CreateWindowExA	CreateWindowExA
USER32DLL WAPI3: SetPropA	SetPropA
USER32DLL WAPI2: GetPropA       GetPropA
USER32DLL WAPI2: RemovePropA    RemovePropA
USER32DLL WAPI4: PostMessageA	PostMessageA
USER32DLL WAPI5: CallWindowProcA CallWindowProcA
USER32DLL WAPI1: GetKeyState-	GetKeyState
USER32DLL WAPI1: OpenClipboard-	OpenClipboard
USER32DLL WAPI1: GetClipboardData- GetClipboardData
USER32DLL WAPI1: CloseClipboard- CloseClipboard
USER32DLL WAPI1: GetWindowTextLengthA GetWindowTextLengthA
KERNEL32DLL WAPI1: GetModuleHandleA GetModuleHandleA 

\- SLEEP KERNEL32DLL WAPI1: DLLSleep Sleep
\- SLEEP : SLEEP DLLSleep DROP ;

USER32DLL WAPI4: SendMessageA	SendMessageA

USER32DLL WAPI3: SetWindowLongPtrA SetWindowLongPtrA

GDI32DLL  WAPI1: GetStockObject	GetStockObject

?#define WM_USER                         0x0400

?#define SBS_SIZEGRIP	16
?#define SB_SETTEXTA	WM_USER 1 +
?#define SB_SETPARTS	WM_USER 4 +
?#define SB_SETTEXTW	WM_USER 11 +

?#define EM_EXLIMITTEXT			WM_USER  53 +

?#define WM_DESTROY	0x0002

?#define WM_PAINT $F
?#define WM_SETFONT	0x0030
?#define WM_GETFONT 49
?#define WM_CONTEXTMENU 123
?#define WM_KEYDOWN 256
?#define WM_KEYUP 257
?#define WM_CHAR 258
?#define WM_TIMER 275
?#define WM_COPY	0x0301
?#define WM_PASTE	0x0302

?#define WM_LBUTTONDBLCLK 515

?#define EM_GETLINE 196

?#define CF_TEXT	1


?#define VK_BACK	8
?#define VK_SHIFT	16
?#define VK_CONTROL	17
?#define VK_RIGHT 39
?#define VK_DOWN	40
?#define VK_NEXT	34
?#define VK_END	35
?#define VK_HOME	36
?#define VK_LEFT	37
?#define VK_UP	38
?#define VK_INSERT	45
?#define VK_DELETE	46

?#define GWLP_WNDPROC	-4

?#define EM_GETSEL	0x00B0
?#define EM_SETSEL	0x00B1
?#define EM_LINEINDEX	0x00BB
?#define EM_LINELENGTH	0x00C1
?#define EM_REPLACESEL	0x00C2
?#define EM_LINEFROMCHAR	0x00C9

?#define WS_EX_CLIENTEDGE	0x00000200

?#define ANSI_FIXED_FONT $B

?#define ES_MULTILINE        0x0004
?#define ES_AUTOVSCROLL      0x0040
?#define ES_AUTOHSCROLL      0x0080

VARIABLE OLDEDITBOXPROC
0 VALUE hwndStatusBar

$100 constant #history			\ -- n ; number of command lines for history
$100 constant MAXSTRING			\ -- n ; line buffer size
#history MAXSTRING * constant /history	\ -- n ; size of history buffer
/history MAXSTRING - constant /hscroll	\ -- n ; size of history buffer less one line
/history MAXSTRING - constant /hcurr	\ -- n ; offset to current line

$100000 constant /reBuffer	\ -- len ; size of RichEdit buffer ; SFP020
$1000 constant /reFree		\ -- len ; size to leave free ; SFP020

\- fifo  $14 constant  fifo
\- int : int 8 field ;
\- 3drop : 3drop NIP 2drop ;
\- C/LINE CREATE C/LINE 64 ,
\- >OUT VARIABLE >OUT
: do-oldeditboxproc-
  oldeditboxproc @ -rot 2>r -rot 2r> CallWindowProcA
;


\- Z" : Z" POSTPONE S"  POSTPONE DROP  ; IMMEDIATE

 0
  16 + \ gen-sid +				\ 00: reuse field names of GEN-SID
  0 field richedit-Ififo		\ 08: KEY queue
  fifo +				\ reuse FIFO field names
  0 field richedit-Ofifo		\ 1C: EMIT queue
  fifo +				\ reuse FIFO field names
  int richedit-dblclkxt			\ 30: xt of double-click action
  int richedit-history			\ 34: points to ALLOCATEd history buffer
  int richedit-history-index		\ 38: current history line#
  int richedit-history-lowest-index	\ 3C: lowest valid history line#
  int richedit-flags			\ 40: allows users/threads to end ACCEPT/KEY
  int re.ForeColor			\ 44: foreground colour in RGB format ; SFP013
  int re.BackColor			\ 48: background colour in RGB format ; SFP013
  int re.hFont				\ 4C: font handle ; SFP017
  int re.TimerID			\ 50: flush timer handle
  int re.Flushing			\ 54: set when flush in progress
  \ -- end of original structure
  int re.ContextXt			\ 58: xt of Context Menu handler
  7 cells +				\ 5C: reserve space
  CONSTANT richedit-sid


$0001 constant re-quit-mask		\ -- mask ; controls quit function
\ $0002 constant re-caret-mask		\ -- mask ; set to stop EMIT going to end
$0004 constant re-CxMenu-mask		\ -- mask ; set for an alternate context menu
$8000 constant re-Rev2-mask		\ set for June 2012 changes


: c>ReFifo-	\ char -- ; send character to Richedit input Fifo
  op-handle @ richedit-Ififo >FIFO(b) drop	\ to input FIFO for KEY
;

: PASTE>FIFO-    \ zaddr *FIFO -- ; SFP019
\ +G Copy 0 terminated string to FIFO. Reformat string as necessary.
  >r
  begin
    dup c@ dup
   while				\ -- addr char
    case
      $0D of
        $0D r@ >FIFO(b) drop		\ issue CR
        dup 1+ c@ $0A =			\ check for trailing LF
        if  1+  endif			\ and ignore it
      endof
      $0A of				\ leading LF for Unix files
        $0D r@ >FIFO(b) drop		\ issue CR
      endof
        r@ >FIFO(b) drop
    end-case
    1+
  repeat
  2drop
  r> drop
;


: do-keyDn-	\ hwnd msg wParam lParam -- status
\ Check cursor and other keys
  over VK_LEFT = if
    23 c>ReFifo-  4drop 0  exit		\ <CTRL><W> to KEY/ACCEPT
  endif
  over VK_RIGHT = if
    18 c>ReFifo-  4drop 0  exit		\ <CTRL><R> to KEY/ACCEPT
  endif
  over VK_UP = if
    5 c>ReFifo-  4drop 0  exit		\ <CTRL><E> to KEY/ACCEPT
  endif
  over VK_DOWN = if
    4 c>ReFifo-  4drop 0  exit		\ <CTRL><D> to KEY/ACCEPT
  endif
  over VK_DELETE = if			\ delete key?
    127 c>ReFifo-  4drop 0  exit
  endif
  over VK_HOME = if			\ <HOME>
    0 c>ReFifo-  71 c>ReFifo-		\ use 0 as lead in char
    4drop  0  exit
  endif
  over VK_END = if			\ <END>
    0 c>ReFifo-  79 c>ReFifo-		\ use 0 as lead in char
    4drop  0  exit
  endif

  over VK_BACK = if			\ delete key?
    4drop 0  exit
  endif

\ Ignore certain control keys which affect RichEdit devices.
  VK_CONTROL GetKeyState- $8000 and if	\ check control keys to ignore
    case  over $FF and
      [CHAR] A of  3drop  EM_SETSEL 0 -1 SendMessageA drop  0 endof	\ select all
      [CHAR] C of  3drop  WM_COPY  0 0 SendMessageA drop  0  endof
      [CHAR] V of  4drop 0  endof	\ need to paste to input
      [CHAR] E of  4drop 0  endof	\ centres under W2000
\      [CHAR] F of  3drop  WM_COMMAND MENUID_FIND 0 SendMessageA drop  0  endof	\ blows up under W2000 without handler
      [CHAR] G of  4drop 0  endof	\ blows up under W2000
      [CHAR] R of  4drop 0  endof	\ right justifies under W2000
                   drop do-oldeditboxproc-	\ SFP003
    end-case
  else					\ -- hwnd msg wParam lParam
    VK_SHIFT GetKeyState- $8000 and	\ check for Shift-Insert ; SFP028...
    VK_INSERT GetKeyState- $8000 and
    and if
      3drop WM_PASTE 0 0 PostMessageA drop	\ paste message
      0
    else
      do-oldEditboxProc-
    endif				\ ...SFP028
  then
;

: do-keyUp-	\ hwnd msg wParam lParam -- status
\ Ignore certain control keys which affect RichEdit devices.
  over VK_BACK = if		\ delete key?
    4drop 0
  else
    VK_CONTROL GetKeyState- $8000 and if	\ check control keys to ignore
      case  over $FF and
        [CHAR] V of  4drop 0  endof	\ need to paste to input
        [CHAR] E of  4drop 0  endof	\ centres under W2000
        [CHAR] F of  4drop 0  endof	\ blows up under W2000 without this
        [CHAR] G of  4drop 0  endof	\ blows up under W2000
        [CHAR] R of  4drop 0  endof	\ right justifies under W2000
                     drop do-oldeditboxproc-	\ SFP003
      end-case
    else
      do-oldEditboxProc-
    then
  then
;

: DoPaste-	{ sid -- }
  sid  @ OpenClipBoard- if
    CF_TEXT GetClipboardData- ?dup if
      sid richedit-Ififo PASTE>FIFO-
    then
    CloseClipboard- drop
  then
;

: clear-history-         \ sid -- ; erase all strings in history buffer
  #history 1- over
 richedit-history-index !
  #history 1- over richedit-history-lowest-index !
  richedit-history @ ?dup
  if  /history erase  then
;

: add-history-line-      \ c-addr u sid -- ; Add line
  over if				\ only add non-blank lines
    dup richedit-history @ if		\ to a buffer that exists
      dup richedit-history-lowest-index @
      1-  0 max				\ update range
      over richedit-history-lowest-index !
      dup richedit-history @		\ scroll buffer by one line
        dup MAXSTRING + swap /hscroll move
      richedit-history @ /hcurr + place	\ place text in last line
      exit
    then
  then
  drop 2drop
;

: add-history-line> 0 PARSE OP-HANDLE @ add-history-line- ;

: (neweditboxproc)-	\ hwnd msg wParam lParam -- stat
  { | text[ 256 ] -- }
  3 pick szSID GetPropA			\ get associated SID for device
  dup op-handle ! ip-handle !		\  and use as default I/O here

  case  2 pick

    WM_KEYDOWN of	        \ -- hwnd msg wParam lParam
      do-keyDn-
    endof

    WM_KEYUP of		        \ -- hwnd msg wParam lParam
      do-keyUp-
    endof

    WM_CHAR of			\ -- hwnd msg wParam lParam
      drop nip				\ hWnd char --
      case  255 and
        1 of  drop  endof		\ CTRL-A
	3 of  drop  endof		\ CTRL-C
        22 of				\ Ctrl-V?
          drop op-handle @ DoPaste-
        endof
          c>ReFifo-  drop   		\ --
      end-case
      0
    endof

    WM_GETFONT of	        \ -- hwnd msg wParam lParam ; SFP017
 \ Edit controls do not respond to WM_GETFONT.
      4drop  op-handle @ re.hFont @
    endof

    WM_SETFONT of	        \ -- hwnd msg wParam lParam ; SFP017
      over op-handle @ re.hFont !  do-oldeditboxproc-
    endof

    WM_DESTROY of		\ -- hwnd msg wParam lParam
      3 pick szSID RemovePropA drop	\ MSDN says you must do this! SFP009
      do-oldeditboxproc-
    endof
   WM_PASTE
 of			\ -- hwnd msg wParam lParam ; SFP028
      4drop  op-handle @ doPaste-  0
    endof

    drop do-oldeditboxproc-              \ call previous handler
  end-case
;
'  (NEWEDITBOXPROC)- WNDPROC: neweditboxproc-

: richedit-open- { unused length hparent sid | initcomctls[ /initcommon ] -- sid ior }

  Z" commdlg_FindReplace" RegisterWindowMessageA  $findmsgstring# ! \  -> findmsgstring#

  /history allocate
  if  drop -1  exit  then
  sid richedit-history !
  sid re.TimerID off

  WS_EX_CLIENTEDGE
  z" richedit"
  ^null
  ES_AUTOVSCROLL
  ES_AUTOHSCROLL or
  ES_MULTILINE or
  ES_WANTRETURN or                      \ edit styles
  ES_NOHIDESEL or                       \ keep the selection when we lose focus
  WS_VISIBLE or
  WS_CHILD or                           \ window styles
  WS_HSCROLL or
  WS_VSCROLL or
  WS_BORDER or
  CW_USEDEFAULT
  CW_USEDEFAULT
  CW_USEDEFAULT
  CW_USEDEFAULT
  hparent
  0 0 GetModuleHandleA 0
  CreateWindowExA ?dup if
    sid  !                    \ store window handle

    sid  @ EM_EXLIMITTEXT 0 /reBuffer SendMessageA drop

    sid  @ GWLP_WNDPROC
 ['] neweditboxproc-
    SetWindowLongPtrA oldeditboxproc !

    sid  @                    \ set SID property to SID itself
    z" SID" sid SetPropA 0= if
      sid -2  exit
    then

    sid clear-history-                   \ erase history buffer

    sid  @                    \ set default font
    WM_SETFONT
    ANSI_FIXED_FONT getstockobject
    1 SendMessageA drop

    sid richedit-Ififo length InitialiseFIFO-
    if  sid -3  exit  then              \ failed to initialise FIFO
    sid richedit-Ofifo length InitialiseFIFO-
    if  sid -3  exit  then              \ failed to initialise FIFO

    sid 0
  else
    sid -1                              \ failed to create richedit window
  then

  $50 C/LINE !

  re-quit-mask sid richedit-flags bic!	\ ensure KEY/ACCEPT quit flag off
\  sid richedit-flags off		\ ensure KEY/ACCEPT quit flag off
;


: richedit-emit?-        \ sid -- flag
  dup  richedit-Ofifo fifo_used @
  swap richedit-Ofifo fifo_size @
  <
;

: toTrim-	\ hwnd -- len
\ Number of bytes to trim from the control.
  GetWindowTextLengthA  /reBuffer /reFree - - 0 max
;

: findClipLine-	{ len hwnd -- len' }
\ Find the character index of the start of the first line to
\ leave alone.
  hwnd EM_LINEFROMCHAR len /reFree + 0 SendMessageA	\ offset with hysteresis to line number
  hwnd EM_LINEINDEX rot 0 SendMessageA	\ get line#
;

: trimText-	{ len hwnd -- }
\ Clip the given amount of text from the start of the text buffer.
  hwnd EM_SETSEL 0 len SendMessageA drop
  hwnd EM_REPLACESEL 0 zNull SendMessageA drop
;

: trimBuffer-	\ hwnd --
\ Trim the text buffer if it is too big.
  dup toTrim- dup if			\ -- hwnd len ; if anything to trim
    over findClipLine-			\ -- hwnd len' ; find clip point
    swap trimText-
  else
    2drop
  endif
;

?#DEFINE EM_SETCHARFORMAT 4
?#DEFINE EM_SETBKGNDCOLOR 4
?#DEFINE CFM_BACKCOLOR	$4000000
?#DEFINE CFM_COLOR	$40000000
?#DEFINE SCF_SELECTION 1

: BackColourSelection	\ colour hwnd -- ; SFP014
  { | cf2[ /charformat2 ] -- }
  /charformat2 cf2[ l!			\ initialise CHARFORMAT2 structure
  CFM_BACKCOLOR
0 cf2[ cf2.dwMask l!
  0 cf2[ cf2.dwEffects l!		\ no effects
  swap cf2[ cf2.crBackColor l!		\ back colour
  dup EM_SETCHARFORMAT SCF_SELECTION cf2[ SendMessageA drop
  EM_SETBKGNDCOLOR 0 cf2[ cf2.crBackColor l@ SendMessageA drop
;

: ForeColourSelection	\ colour hwnd -- ; SFP013
  { | cf[ /charformat2 ] -- }
  /charformat2 cf[ l!			\ initialise CHARFORMAT structure
  CFM_COLOR cf[ cf2.dwMask l!		\ foreground color is valid
  0 cf[ cf2.dwEffects l!		\ no effects
  swap cf[ cf2.crTextColor l!		\ text colour
  EM_SETCHARFORMAT SCF_SELECTION cf[ SendMessageA drop
;


: richedit-flushOP-      { sid | hwnd *fifo -- ior }	\ SFP013
  sid 0=				\ check for valid SID ; SFP026
  if  0 exit  endif
  sid  @			\ check for valid handle
  dup 0= if  drop 0 exit  endif
  dup -1 = if  drop 0 exit  endif
  -> hwnd
  sid richedit-Ofifo			\ check for valid FIFO
  dup 0= if  drop 0 exit  endif
  -> *fifo

  sid re.Flushing @ 0= if
    sid re.Flushing on
    *fifo FIFO? if
      hwnd trimBuffer-			\ check for buffer overflow ; SFP020
      *fifo fifo_memptr @
      *fifo fifo_used @ +
      0 swap c!				\ turn it into a zero terminated string
      hwnd EM_SETSEL -1 -1 SendMessageA drop

      hwnd EM_REPLACESEL FALSE *fifo fifo_memptr @ SendMessageA drop
      *fifo fifo_memptr @
      dup *fifo fifo_head !
          *fifo fifo_tail !
      0   *fifo fifo_used !
    endif
    sid re.Flushing off
  then
  0
;

: (richedit-emit)-       \ character sid -- ; A "NORMAL" emit case
\  over 7 = if    2drop  REbeep	  exit  then
  dup richedit-emit?- 0= if
    dup richedit-FLUSHOP- drop
  then
  over $D = if				\ SFP029
    >OUT off
  else
    1 >OUT +!
  endif
  richedit-Ofifo >fifo(b) drop
;


: GetCurrLine-	\ caddr len sid --
\ The returned line includes the trailing CR/LFs.
  { caddr len sid | hRE -- }
  sid  @ -> hRE
  len 1- caddr w!
  hRE EM_LINEFROMCHAR -1 0 SendMessageA		\ line of current selection
  hRE EM_GETLINE rot caddr SendMessageA	 	\ ptr -> 0 terminated Line
  0 swap caddr + c!				\ zero terminate line
;

: stripCRLF-	\ caddr len -- caddr len'
\ remove last character if CR or LF.
  dup if
    2dup + 1- c@				\ last character
    dup 10 = swap 13 = or 			\ CR or LF
    if  1-  2dup + 0 swap c!  endif		\ SFP027
  endif
;

: replace-last-line-	\ caddr u offset sid -- ; replace last line
\ Place caddr u text on current line of device at starting position offset.
  { | hwndE text[ 256 ] start length spos -- }
   @ -> hwndE					\ edit box handle
  -> spos						\ starting position offset ; SFP007
  255 min text[ zplace					\ make zero terminated
  hwndE EM_SETSEL -1 -1 SendMessageA drop		\ end of display
  hwndE EM_LINEINDEX -1 0 SendMessageA -> start		\ find start of line
  hwndE EM_LINELENGTH start 0 SendMessageA -> length	\ find length of line
  hwndE EM_SETSEL start spos + length spos - bounds SendMessageA drop	\ select line ; SFP007
  hwndE EM_REPLACESEL FALSE text[ SendMessageA drop	\ replace with new string
;

: (reBS)-	\ sid -- ; destructive backspace operation
  { sid | buff -- }
  sid richedit-FLUSHOP- drop			\ flush any pending output
  1024 allocate if				\ get 1k buffer
    drop exit
  then
  -> buff
  buff 1024 sid GetCurrLine-			\ get current line of text
  buff zcount					\ -- caddr len
  stripCRLF- stripCRLF- stripCRLF- stripCRLF-	\ remove trailing CR and LF ; SFP027
  dup if  >OUT @ if  >OUT decr  endif  endif	\ deal with OUT ; SFP027 SFP029
  1- 0 max 0 sid replace-last-line-		\ shorten and replace
  buff free drop
;


: richedit-emit-         \ character sid -- ; SFP029
  tuck  over 8 <>			\ backspace?
  if  (richedit-emit)-  else  nip  (reBS)-  then
  >OUT @ 1022 u> if
    $A over (richedit-emit)-
    >OUT off  op-line# incr
  endif
  DROP \  ?setReTimer
;


: richedit-cr-           \ sid --
  $a swap richedit-emit-
  >OUT off op-line# incr
\  emptyIdle				\ force output ; SFP010
;

\ ... SFP023

: richedit-type-         \ address length sid --
  -rot  bounds ?do
    i c@ over richedit-emit-
  loop
  drop
;

H-STDOUT VALUE CONS-STDOUT

variable sttstv

0 VALUE BARbuff

: BARTYPE BARbuff zAppend ;
: BAREMIT BARbuff zcount + W! ;

: UpdateStatusBar
  { | buff[ 256 ] STYPE SEMIT  -- }
    ['] TYPE DEFER@ -> STYPE
    ['] EMIT DEFER@ -> SEMIT

\    ['] TYPE1 TO TYPE
\    ['] EMIT_M TO EMIT
     CONS-STDOUT TO H-STDOUT  .S CR
     0 TO H-STDOUT

    ['] BARTYPE TO TYPE
    ['] BAREMIT TO EMIT
   buff[ TO BARbuff

    base @ >r
    S" Base: " buff[ zplace  BASE @ decimal .

  hwndStatusbar SB_SETTEXTA 0 buff[ SendMessageA drop

  s" Depth: " buff[ zplace  DEPTH .

  hwndStatusbar SB_SETTEXTA 1 buff[ SendMessageA drop

  s" Context: " buff[ zplace  CONTEXT L@ VOC-NAME.
  hwndStatusbar SB_SETTEXTA 2 buff[ SendMessageA drop

  s" Current: " buff[ zplace	GET-CURRENT VOC-NAME.
  hwndStatusbar SB_SETTEXTA 3 buff[ SendMessageA drop

  s" Tt: " buff[ zplace	C/LINE @ .
  hwndStatusbar SB_SETTEXTA 4 buff[ SendMessageA drop

   R> base !  SEMIT TO EMIT  STYPE TO TYPE
;

: richedit-key?-         \ sid -- flag
  dup richedit-flags @ re-quit-mask and	\ check bail out flag
  if  drop -1 exit  then
  busyidle-
  dup richedit-FLUSHOP- drop
  richedit-Ififo FIFO? 0<>
;

: richedit-key-          \ sid -- char
\  UpdateStatusBar
  begin
    dup richedit-flags @ re-quit-mask and \ check bail out flag
    if  drop $D  exit  then
    dup richedit-key?- 0=		\ poll for key
  while
    20 SLEEP \ ms
  emptyidle-			\ give Windows message loop a chance ; SFP016 SFP021
  repeat
  richedit-Ififo FIFO>(b) drop

;

: backup-last-line-	\ backup sid -- ; set cursor backup chars from end
   @ >r				\ edit box handle
  r@ EM_SETSEL -1 -1 SendMessageA drop		\ go to end of display
  r@ EM_LINEINDEX -1 0 SendMessageA 		\ find start of line
  r@ EM_LINELENGTH 2 pick 0 SendMessageA		\ find length of line
  +  swap -					\ backup start len -- pos
  r> EM_SETSEL rot dup SendMessageA drop		\ select line
;

: restart-history-	\ sid -- ; reinitialise current index (ACCEPT started)
  #history 1- swap richedit-history-index !
;

: hLine-		\ sid line# -- addr
  MAXSTRING *  swap richedit-history @ +
;

: step-history-  \ sid step --
  over richedit-history-index @ +	\ update current line# and range check
  over richedit-history-lowest-index @ max  #history 1- min
  swap richedit-history-index !
;

: curr-history-	\ sid -- c-addr u
  dup richedit-history @ if
    dup richedit-history-index @ hLine-
  else
    drop cNull
  then
  count
;

: at-last-history?-	\ sid -- flag ; true if at last history line
  richedit-history-index @ #history 1- =
;


: richedit-ACCEPT-	\ address maxlen sid -- len'
  { address maxlen sid | len backup spos -- len' }

  0 -> len				\ #chars read to buffer
  0 -> backup				\ cursor offset from END of line
  >OUT @ -> spos				\ number of characters already on line ; SFP007

  sid restart-history-			\ history starts with last line used

  begin
    sid richedit-key-
\  DUP X['] EMIT EXECUTE
    dup $D <>  len maxlen <> and
   while
    dup 9 =				\ convert <TAB> to <SPACE>
    if  drop bl  then

    case

      5 of   ( <CTRL><E> previous history line )
        sid curr-history-		\ get current history line
        dup -> len  0 -> backup		\ reset length and cursor offset
        2dup address swap move		\ copy to buffer
	spos sid replace-last-line-	\ and replace the line ; SFP007
	sid -1 step-history-		\ step to previous line
      endof

      4 of   ( <CTRL><D> next history line )
          sid at-last-history?- if	\ clear line if already at last
	    0 -> len  0 -> backup
	    address 0 spos sid replace-last-line-	\ SFP007
	  else				\ otherwise get next
            sid 1 step-history-		\ get next history-line
            sid curr-history-		\ get next history line
            dup -> len  0 -> backup	\ reset length and cursor offset
            2dup address swap move	\ copy to buffer
            spos sid replace-last-line-	\ and replace the line ; SFP007
	  endif
      endof

      24 of  ( <CTRL><X> do nothing )
      endof

      23 of  ( <CTRL><W> - step left )
        backup 1+  len min  -> backup
	backup sid backup-last-line-
      endof
      18 of  ( <CTRL><R> - step right )
        backup 1-  0 max  -> backup
	backup sid backup-last-line-
      endof

      8 of   ( <BACKSPACE> - delete before )
        len backup - 0> if
	  backup if
	    address len + backup -	\ cursor position
	    dup 1- backup cmove		\ copy tail down one character
	  then
          len 1- -> len
	  address len spos sid replace-last-line-	\ SFP007
          backup if
            backup len min -> backup
            backup sid backup-last-line-
          then
        then
      endof

      $7f of   ( <DELETE> - delete after )
        len if			        \ if there's anything to delete ; SFP006
          len backup - 0< 0= if		\ if there's a tail
            backup 1 > if
	      address len + backup -	\ cursor position = dest
	      dup 1+ swap		\ start from cursor +1
	      backup 1- cmove		\ copy tail down one character
	    then
            len 1- -> len		\ cut length of line
	    backup 1- 0 max -> backup	\ tail is now one char shorter
	    address len spos sid replace-last-line-	\ SFP007
            backup if
              backup len min -> backup
              backup sid backup-last-line-
            then
          then
        then				\ SFP006
      endof

      0 of	\ 0 is lead in character
        case  sid richedit-key-
	  $47 of  len -> backup  endof	\ <HOME>  go to start of line
	  $4F of  0 -> backup    endof	\ <END>   go to end of line
	endcase
        address len spos sid replace-last-line-	\ SFP007
	backup if
          backup sid backup-last-line-
	then
      endof

        backup if
	  address len + backup -	\ cursor position
\	  dup  dup   DUP CCTT0 !
\ 1+  backup DUP CCTT1 ! VFX_cmove>	\ make space for one character
	  dup  dup 1+ backup cmove>	\ make space for one character
	  c!				\ insert character
	else
          address len + c!
	then
        len 1+ -> len
        address len spos sid replace-last-line-	\ SFP007
	backup if
          backup sid backup-last-line-
	then

    end-case

    spos len + backup - >OUT !		\ update OUT ; SFP008

  repeat
  drop

  sid richedit-flags @ re-quit-mask and 0= if
    bl address len + c!  >OUT incr		\ add trailing space to o/p ; SFP008
    address len 1+ spos sid replace-last-line-	\ SFP007
    address len sid add-history-line-	\ update history
  then

 len
;

