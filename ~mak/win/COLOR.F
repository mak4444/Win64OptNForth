
: RGB           ( red green blue -- colorref )
                2 \ PC_NOCOLLAPSE   \ flag             ( for NON-palette rgb value )
                256 * +         \ flag*256 + blue
                256 * +         \ flag*256 + blue*256 + green
                256 * + ;       \ flag*256 + blue*256 + green*256 + red

: COLOR>
 RGB
 0 \ PS_SOLID  PenStyle
 1 \ penwidth
 ROT
 RCreatePen ;

: COLOR:
  COLOR> VALUE
;

  0   0   0  COLOR: ^BLACK
 64  64  64  COLOR: ^DKGRAY
128   0   0  COLOR: ^RED
255   0   0  COLOR: ^LTRED
  0 128   0  COLOR: ^GREEN
  0 255   0  COLOR: ^LTGREEN
  0   0 128  COLOR: ^BLUE
  0   0 255  COLOR: ^LTBLUE
128 128   0  COLOR: ^YELLOW
255 255   0  COLOR: ^LTYELLOW
128   0 128  COLOR: ^MAGENTA
255   0 255  COLOR: ^LTMAGENTA
  0 128 128  COLOR: ^CYAN
  0 255 255  COLOR: ^LTCYAN
128 128 128  COLOR: ^GRAY
255 255 255  COLOR: ^WHITE
192 192 192  COLOR: ^LTGRAY
128  64   0  COLOR: ^BROWN

: PenColor  (  hDC color_object  -- )
  RSelectObject DROP ;

