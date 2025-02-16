[IFNDEF] USER32DLL
Z" USER32.DLL" DLL_L
DUP 0= [IF] .( USER32.DLL LOAD ERROR) ABORT [THEN]
CONSTANT USER32DLL
[THEN]

\- DEFER : DEFER VECT ;

USER32DLL WAPI1: TranslateMessage	TranslateMessage

   0
   8 FIELD msg.hWnd
   8 FIELD msg.Message
   8 FIELD msg.wParam
   8 FIELD msg.lParam
   8 FIELD msg.time
   8 FIELD msg.point
\  _dword   a-field msg.lPrivate

CONSTANT /MSG



\- PM_REMOVE #define PM_REMOVE 1

USER32DLL WAPI5: PeekMessageA	PeekMessageA
USER32DLL WAPI1: DispatchMessageA	DispatchMessageA

: (BusyIdle)-    { | WinMessage[ /MSG ] -- }
  WinMessage[ 0 0 0 PM_REMOVE PeekMessageA if
\    WinMessage[ CheckModeless- 0= if	\ SFP001
      WinMessage[ TranslateMessage drop
      WinMessage[ DispatchMessageA drop
\    then
  then
;
DEFER BUSYIDLE-

' (BusyIdle)- TO BUSYIDLE-

: (EmptyIdle)-	{ | WinMessage[ /MSG ] -- }
  begin
    WinMessage[ 0 0 0 PM_REMOVE PeekMessageA
   while
\    WinMessage[ CheckModeless- 0= if	\ SFP001
      WinMessage[ TranslateMessage drop
      WinMessage[ DispatchMessageA drop
\    then
  repeat
;

DEFER EMPTYIDLE-

' (EMPTYIDLE)- TO EMPTYIDLE-
