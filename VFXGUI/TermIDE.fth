
REQUIRE OFD_STR ~mak\fdialog.4 

[IFNDEF] COMCTL32DLL
Z" Comctl32.DLL" DLL_L
DUP 0= [IF] .( Comctl32.DLL LOAD ERROR) ABORT [THEN]
CONSTANT COMCTL32DLL
[THEN]
USER32DLL WAPI2: GetWindowRect- GetWindowRect
USER32DLL WAPI3: InvalidateRect- InvalidateRect
COMCTL32DLL WAPI4: CreateStatusWindow- CreateStatusWindow
GDI32DLL WAPI2: GetTextMetricsA GetTextMetricsA
USER32DLL WAPI4: DefWindowProcA DefWindowProcA
USER32DLL WAPI1: GetDC	GetDC
USER32DLL WAPI2: ReleaseDC	ReleaseDC
USER32DLL WAPI1: SetFocus	SetFocus
USER32DLL WAPI6: MoveWindow	MoveWindow
USER32DLL WAPI4: MessageBoxA	MessageBoxA

0 VALUE hWndTerminal
0 VALUE hWndToolbar

0xFFFFFFF2 constant GCLP_HICON
0xFFFFFFDE constant GCLP_HICONSM

#define IDW_MAINWINDOW  $FEED
#define IDI_VFXICO $FEEE

?#define WM_SIZE	0x0005
?#define WM_SETFOCUS	0x0007
?#define WM_CLOSE	0x0010
?#define WM_COMMAND	0x0111

?#define WM_CHAR      $102

?#define MB_OK	0x000000

CREATE idetm TEXTMETRIC ALLOT

: ps,		\ offset width -- offset'
  + dup l,
;

create parts[	\ -- addr ; parts structure for status bar
0
  50 ps,	60 ps,		120 ps,	120 ps,
  40 ps,	50 ps,		35 ps,	55 ps,
  100 ps,
drop  -1 l,

9 constant #parts

\- WantStatusbar? $DEAD value WantStatusbar?	\ -- n ; nonzero to have status bar
\ *G This value must be non-zero (by default $DEAD) to enable the
\ ** console status bar.

: +StatusBar-	\ --
\ *G Show the status bar on the main console, regardless of the
\ ** state of *\fo{WANTSTATUSBAR?} and only if *\fo{HWNDSTATUS} is zero.
  hwndStatusbar if  exit  then
  
  WS_CHILD WS_VISIBLE or  SBS_SIZEGRIP or
  zNull
  &hWndMain @
  WantStatusbar?
  CreateStatusWindow-   TO hwndStatusBar
 hwndStatusBar SB_SETPARTS #parts parts[
 SendMessageA drop

;

: AdjustC/Line-  \ hwnd width --

  over GetDC                    \ hwnd width hdc --
  dup idetm GetTextMetricsA

 drop
  rot swap ReleaseDC
 drop       \ width --
  idetm TEXTMETRIC.AveCharWidth L@
  idetm TEXTMETRIC.MaxCharWidth L@ + 2/

  ?dup if / else drop 1 then

  $3F * $40 /  2*
  c/line !
;

: GetWindowHeight-	\ hwnd -- height
  { | r[ /RECT ] -- }
  r[ GetWindowRect- drop
  r[ dup RECT.bottom @  swap RECT.top @ -
;

\- FileDLG : FileDLG ( -- adr len )  OFD_STR GetOpenFileNameA DROP LPSTRFILE ZCOUNT ;

: RICH_TYPE ( adr len -- )  0 DO  COUNT  hWndRichEdit WM_CHAR ROT 0 SendMessageA DROP LOOP DROP ;
: ZRICH_TYPE ( zadr  -- )
  BEGIN  COUNT DUP
  WHILE   hWndRichEdit WM_CHAR ROT 0 SendMessageA DROP
  REPEAT 2DROP ;


?#define WM_CREATE	1
0 VALUE main_width  
0 VALUE main_height

: SP-DO

  hWndRichEdit \ 0x4BD7F0 @ 
 0 0
 main_width
 main_height  vaultSIZE 3 + -
       TRUE
 MoveWindow
 drop
	hWndRichEdit  \ 0x4BD7F0 @ 
 main_width  AdjustC/Line-

        vaultOFFSET vaultSIZE + TO vaultSIZE 0 TO vaultOFFSET

    	vaultWnd
	0 main_height vaultSIZE  -
	main_width vaultSIZE
	1
	MoveWindow DROP

    	SP_hwnd
	0 main_height vaultSIZE - 3 -
	main_width 3
	1
	MoveWindow DROP

;
: WM_SIZE_DO
  { hWnd Msg wParam lParam -- }

      hWndToolBar dup if
        dup 0 0 lparam LOWORD lparam HIWORD TRUE MoveWindow drop
        GetWindowHeight-
      then				\ returns 0 or height of toolbar

      hwndStatusbar dup if
        dup WM_SIZE 0 0 SendMessageA drop
	GetWindowHeight-
      then				\ returns 0 or height of statusbar

  +
 lparam HIWORD - NEGATE TO main_height
 lparam LOWORD TO main_width
\ TO RRRR   \ -- htool + hstatus
  SP-DO
;

: MainWindowProc-        \ hWnd Msg wParam lParam -- res
( *G The winproc of the main console window. )
  { hWnd Msg wParam lParam | res -- res }

  V-CONS-STDOUT TO H-STDOUT
\ CR   ." hwnd=" hwnd H. ." Msg=" Msg H. 
\      ." wParam=" wparam H. ." lParam=" lParam H.


     0 TO H-STDOUT

\   TlsIndex>
  0 -> res
\  sttstdo
  Msg case
    WM_CLOSE    of
  INIFILE$- COUNT  W/O CREATE-FILE 0= IF
  H-STDOUT >R  TO H-STDOUT
  OP-HANDLE @
  #history 0 do

    dup i hLine- count DUP
    IF  ." add-history-line> "  type cr
    ELSE 2DROP
    THEN
  loop DROP

 H-STDOUT CLOSE-FILE DROP
 R> TO H-STDOUT 
  THEN
  SP@ SP0 !
\+ GRBYE GRBYE
  bye
  endof

  WM_SETFOCUS
 of  hWndTerminal SetFocus drop  endof


     WM_SIZE   of \ hWnd Msg wParam lParam WM_SIZE_DO

      hWndToolBar dup if
        dup 0 0 lparam LOWORD lparam HIWORD TRUE MoveWindow drop
        GetWindowHeight-
      then				\ returns 0 or height of toolbar

      hwndStatusbar dup if
        dup WM_SIZE 0 0 SendMessageA drop
	GetWindowHeight-
      then				\ returns 0 or height of statusbar

  +
 lparam HIWORD - NEGATE TO main_height
 lparam LOWORD TO main_width
\ TO RRRR   \ -- htool + hstatus
  SP-DO


    endof
     $7777   of
		vaultOFFSET IF	SP-DO THEN
\    hWnd Msg wParam lParam DefWindowProcA -> res

    endof

     $14   of  SP-DO  endof
   WM_COMMAND of  
	wParam
	CASE
	cmdInclude  OF FileDLG  LPSTRFILE  ZRICH_TYPE \  Z"  INCLUDED" ZRICH_TYPE
    hWndRichEdit SetFocus drop 

\ 0 LPSTRFILE Z" WM_COMMAND" MB_OK MessageBoxA drop
   ENDOF
	cmdBye   OF
\+ GRBYE GRBYE
 BYE
 ENDOF
	cmdRun   OF \  0 Z" cmdRun" Z" WM_COMMAND" MB_OK MessageBoxA drop

     vaultSaveScript

\    hWndRichEdit WM_CHAR 'Z' 0 SendMessageA DROP
    1 TO ScriptRUN?
    hWndRichEdit WM_CHAR $D 0 SendMessageA DROP

 ENDOF

 	ENDCASE
	0	
   ENDOF

   WM_CREATE OF
		hwnd vault_CREATE
		hwnd Splitter_CREATE
		vaultLoadScript
\		vault_LOAD
   ENDOF


    drop
    hWnd Msg wParam lParam DefWindowProcA -> res

  end-case
  res

;

' MAINWINDOWPROC- WNDPROC: WINlpfnWndProc

CREATE windowplace
  $2C L, \ Length
 0    L, \ flags
 1    L, \ showCmd
 -1   L, \ ptMinPosition POINT.x
 -1   L, \ ptMinPosition POINT.y
 $46  L, \ ptMaxPosition POINT.x
 $1b  L, \ ptMaxPosition POINT.y
 $ad  L, \ rcNormalPos RECT.left
 $57  L, \ rcNormalPos RECT.top
 $337 L, \ rcNormalPos RECT.right 
 $202 L, \ rcNormalPos RECT.bottom 

