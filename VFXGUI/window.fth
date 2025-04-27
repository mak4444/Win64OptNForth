
USER32DLL WAPI1: RegisterClassA RegisterClassA
USER32DLL WAPI2: LoadIconA	LoadIconA
USER32DLL WAPI4: MessageBoxA	MessageBoxA
USER32DLL WAPI2: SetWindowPlacement- SetWindowPlacement

USER32DLL WAPI0: CreateMenu	CreateMenu
USER32DLL WAPI0: CreatePopupMenu	CreatePopupMenu
USER32DLL WAPI4: AppendMenuA	AppendMenuA

?#define MF_STRING	0x00000000
?#define MF_POPUP	0x00000010


?#define CS_VREDRAW          0x0001
?#define CS_HREDRAW          0x0002

?#define MB_OK	0x000000

\- IMAGE-BASE 0 GetModuleHandleA CONSTANT IMAGE-BASE

 create SZBUILDCLASSNAME        s" MPE_1A" s, 0 c,

  0
  4        field IWINDOW.dwExStyle
  4        field IWINDOW.dwStyle
  4        field IWINDOW.dwClassStyle
  4        field IWINDOW.dwIcon
  4        field IWINDOW.dwCursor
  4        field IWINDOW.dwBrush
  4        field IWINDOW.dwClassAtom
  4        field IWINDOW.dwPositionX
  4        field IWINDOW.dwPositionY
  4        field IWINDOW.dwWidth
  4        field IWINDOW.dwHeight
  4        field IWINDOW.dwMenu
  MAX_PATH field IWINDOW.szCaption
  MAX_PATH field IWINDOW.szWndProcName
  $20        field IWINDOW.*callback

 CONSTANT IWINDOW+


HERE $F + $F ANDC DP !
CREATE buildWINDOW- IWINDOW+ ALLOT
buildWINDOW- IWINDOW+ ERASE


: +c!                           \ value addr --
  dup c@ rot + swap c!
;

\- chars : chars ;

: MAKECLASSNAMEUNIQUE-   \ --
( +G An Internal word to make the Forth Window ClassName unique ready   )
( +* for it's next useage.                                              )
  szBuildClassName 5 chars +
  1 over +c!
  c@ [char] Z > if
    1 szBuildClassName 4 chars + +c!
    [char] A szBuildClassName 5 chars + c!
  then
;

: DUP(BUILDCLASS)-	\ *resource -- atom
( +G Given a pointer to the internal IWINDOW structure attempt to       )
( +* create a new Window Class.                                         )
  { | BuildWndClass[ /WNDCLASS- ] }

\  dup ResolveWndProc

  BuildWNDCLASS[ /WNDCLASS- erase

 CS_HREDRAW CS_VREDRAW or  BuildWNDCLASS[ WNDCLASS.style l!

 ['] WINlpfnWndProc       BuildWNDCLASS[ WNDCLASS.lpfnWndProc !

  0 GetModuleHandleA	BuildWNDCLASS[ WNDCLASS.hInstance !

    0   $7F00 LoadIconA	BuildWNDCLASS[ WNDCLASS.hIcon !

  szBuildClassName	BuildWNDCLASS[ WNDCLASS.lpszClassName !

  BuildWNDCLASS[ RegisterClassA		\ -- atom

  MakeClassnameUnique-

  dup 0= if
    HWND_DESKTOP Z" Failed to create class" Z" HDBG" MB_OK MessageBoxA drop
  then
;


: GUI_CreateMainMenu ( -- hmenu )

   CreateMenu   >R

  CreatePopupMenu >R

 R@ MF_STRING cmdInclude Z" &Include"	AppendMenuA DROP \  APPENDM DROP
 R@ MF_STRING cmdBye	Z" &BYE"	AppendMenuA DROP

 R>  R@  MF_POPUP  ROT Z" &File" AppendMenuA DROP


 R@ MF_STRING cmdRun	Z" &Script"	AppendMenuA DROP


 R>
;

: CREATEPARENTWINDOW-	\  -- handle
( +G Given a IWINDOW structure create the Window as a PARENT            )
	0	\ Exstyle
 DUP(BUILDCLASS)- \ ClassAtom
	z" Kaskod Forth"
	$94CF0000 \ Style
	0	\ PositionX
	0 	\ PositionY
	$64	\ Width
	$64	\ Height
 0
  GUI_CreateMainMenu
 IMAGE-BASE
 0 CreateWindowExA
  dup 0= if
    0 Z" Failed to create window" Z" HDBG" MB_OK MessageBoxA drop
  then
;

