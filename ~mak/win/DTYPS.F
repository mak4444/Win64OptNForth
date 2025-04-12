BASE @

HEX
\ префикс     категория
\ CS          опция стиля класса
\ IDI         ид.номер иконки
\ IDC         ид.номер курсора
\ WS          стиль окна
\ CW          опция создания окна
\ WM          сообщение окна
\ SND         опция звука
\ DT          опция рисования текста

\- IMAGE-BASE 0 GetModuleHandleA CONSTANT IMAGE-BASE

IMAGE-BASE CONSTANT HINST  \ Instance текущего приложения
       0 CONSTANT HCON   \ hwnd главного окна приложения
       2 CONSTANT CS_HREDRAW
       1 CONSTANT CS_VREDRAW
       8 CONSTANT CS_DBLCLKS
      20 CONSTANT CS_OWNDC
 8000000 CONSTANT CW_USEDEFAULT
    6004 CONSTANT WS_95
    7F00 CONSTANT IDI_APPLICATION
    7F04 CONSTANT IDI_ASTERISK
    7F03 CONSTANT IDI_EXCLAMATION
    7F01 CONSTANT IDI_HAND
    7F02 CONSTANT IDI_QUESTION
       5 CONSTANT COLOR_WINDOW

(
: #define
  CREATE 0 0 BL WORD COUNT
  >NUMBER DUP IF 1- SWAP 1+ SWAP >NUMBER THEN 2DROP D>S ,
  DOES> @
;
)

#define WS_OVERLAPPED       0x00000000
#define WS_POPUP            0x80000000
#define WS_CHILD            0x40000000
#define WS_MINIMIZE         0x20000000
#define WS_VISIBLE          0x10000000
#define WS_DISABLED         0x08000000
#define WS_CLIPSIBLINGS     0x04000000
#define WS_CLIPCHILDREN     0x02000000
#define WS_MAXIMIZE         0x01000000
#define WS_CAPTION          0x00C00000     \ WS_BORDER | WS_DLGFRAME  */
#define WS_BORDER           0x00800000
#define WS_DLGFRAME         0x00400000
#define WS_VSCROLL          0x00200000
#define WS_HSCROLL          0x00100000
#define WS_SYSMENU          0x00080000
#define WS_THICKFRAME       0x00040000
#define WS_GROUP            0x00020000
#define WS_TABSTOP          0x00010000

#define WS_MINIMIZEBOX      0x00020000
#define WS_MAXIMIZEBOX      0x00010000

#define WS_OVERLAPPEDWINDOW 0x00CF0000

#define WS_TILED            WS_OVERLAPPED
#define WS_ICONIC           WS_MINIMIZE
#define WS_SIZEBOX          WS_THICKFRAME
#define WS_TILEDWINDOW      WS_OVERLAPPEDWINDOW

\ /*
\  * Extended Window Styles   \ добавлено EX_ 16-МАР-2000
\
\  */

#define WS_EX_DLGMODALFRAME     0x00000001
#define WS_EX_NOPARENTNOTIFY    0x00000004
#define WS_EX_TOPMOST           0x00000008
#define WS_EX_ACCEPTFILES       0x00000010
#define WS_EX_TRANSPARENT       0x00000020
\ (WINVER >= 0x0400)
#define WS_EX_MDICHILD          0x00000040
#define WS_EX_TOOLWINDOW        0x00000080
#define WS_EX_WINDOWEDGE        0x00000100
#define WS_EX_CLIENTEDGE        0x00000200
#define WS_EX_CONTEXTHELP       0x00000400

#define WS_EX_RIGHT             0x00001000
#define WS_EX_LEFT              0x00000000
#define WS_EX_RTLREADING        0x00002000
#define WS_EX_LTRREADING        0x00000000
#define WS_EX_LEFTSCROLLBAR     0x00004000
#define WS_EX_RIGHTSCROLLBAR    0x00000000

#define WS_EX_CONTROLPARENT     0x00010000
#define WS_EX_STATICEDGE        0x00020000
#define WS_EX_APPWINDOW         0x00040000

0 
4 FIELD RECT.left
4 FIELD RECT.top
4 FIELD RECT.right
4 FIELD RECT.bottom
CONSTANT /RECT


\ структура аля paint
0 
4 FIELD PS.hdc
4 FIELD PS.fErase
4 FIELD PS.rcPaint
4 FIELD PS.fRestore
4 FIELD PS.fIncUpdate
4 FIELD PS.rgbReserved  31 +
CONSTANT /PS

0
4 FIELD POINT.a
4 FIELD POINT.b
CONSTANT /POINT

\  Mapping Modes from Wingdi.h
#define MM_TEXT             1
#define MM_LOMETRIC         2
#define MM_HIMETRIC         3
#define MM_LOENGLISH        4
#define MM_HIENGLISH        5
#define MM_TWIPS            6
#define MM_ISOTROPIC        7
#define MM_ANISOTROPIC      8

#define WHITE_BRUSH         0
#define LTGRAY_BRUSH        1
#define GRAY_BRUSH          2
#define DKGRAY_BRUSH        3
#define BLACK_BRUSH         4

#define NULL_BRUSH          5
#define HOLLOW_BRUSH        NULL_BRUSH
#define WHITE_PEN           6
#define BLACK_PEN           7

#define NULL_PEN            8
#define OEM_FIXED_FONT      10
#define ANSI_FIXED_FONT     11
#define ANSI_VAR_FONT       12
#define SYSTEM_FONT         13
#define DEVICE_DEFAULT_FONT 14
#define DEFAULT_PALETTE     15
#define SYSTEM_FIXED_FONT   16
#define DEFAULT_GUI_FONT    17

\  /* WINVER >= 0x0400 */


#define WM_NULL                         0x0000
#define WM_CREATE                       0x0001
#define WM_DESTROY                      0x0002
#define WM_MOVE                         0x0003
#define WM_SIZE                         0x0005
#define WM_ACTIVATE                     0x0006

#define WM_SETFOCUS                     0x0007
#define WM_KILLFOCUS                    0x0008
#define WM_ENABLE                       0x000A
#define WM_SETREDRAW                    0x000B
#define WM_SETTEXT                      0x000C
#define WM_GETTEXT                      0x000D
#define WM_GETTEXTLENGTH                0x000E
#define WM_PAINT                        0x000F
#define WM_CLOSE                        0x0010
#define WM_QUERYENDSESSION              0x0011
#define WM_QUIT                         0x0012
#define WM_QUERYOPEN                    0x0013
#define WM_ERASEBKGND                   0x0014
#define WM_SYSCOLORCHANGE               0x0015
#define WM_ENDSESSION                   0x0016
#define WM_SHOWWINDOW                   0x0018
#define WM_WININICHANGE                 0x001A

#define WM_INITDIALOG                   0x0110
#define WM_COMMAND                      0x0111
#define WM_SYSCOMMAND                   0x0112
#define WM_TIMER                        0x0113
#define WM_HSCROLL                      0x0114
#define WM_VSCROLL                      0x0115
#define WM_INITMENU                     0x0116
#define WM_INITMENUPOPUP                0x0117
#define WM_MENUSELECT                   0x011F
#define WM_MENUCHAR                     0x0120
#define WM_ENTERIDLE                    0x0121

#define IDC_ARROW           32512
#define IDC_IBEAM           32513
#define IDC_WAIT            32514
#define IDC_CROSS           32515
#define IDC_UPARROW         32516
#define IDC_SIZE            32640
#define IDC_ICON            32641
#define IDC_SIZENWSE        32642
#define IDC_SIZENESW        32643
#define IDC_SIZEWE          32644
#define IDC_SIZENS          32645
#define IDC_SIZEALL         32646
#define IDC_NO              32648
#define IDC_APPSTARTING     32650

BASE !
