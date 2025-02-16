
: a-field	\ offset size -- offset'
\ *G Equivalent to *\fo{FIELD} but aligns the start of the item to
\ ** the required size. Alignment is only applied to items of size
\ ** 2, 4, 8 and 16 bytes.
  case dup
    2 of  swap 1+ -2 and swap  endof
    4 of  swap 3 + -4 and swap  endof
    8 of  swap 7 + -8 and swap  endof
    16 of  swap 15 + -16 and swap  endof
  endcase
  field
;


0
  4 FIELD initcommon.dwsize \ size of this structure
  4 FIELD initcommon.dwicc  \ flags indicating which classes to be initialized
CONSTANT /initcommon  \ should be initcommoncontrolsex but this is also an API name

 0
  4 field TEXTMETRIC.Height
  4 field TEXTMETRIC.Ascent
  4 field TEXTMETRIC.Descent
  4 field TEXTMETRIC.InternalLeading
  4 field TEXTMETRIC.ExternalLeading
  4 field TEXTMETRIC.AveCharWidth
  4 field TEXTMETRIC.MaxCharWidth
  4 field TEXTMETRIC.Weight
  4 field TEXTMETRIC.Overhang
  4 field TEXTMETRIC.DigitizedAspectX
  4 field TEXTMETRIC.DigitizedAspectY
  1 field TEXTMETRIC.FirstChar
  1 field TEXTMETRIC.LastChar
  1 field TEXTMETRIC.DefaultChar
  1 field TEXTMETRIC.BreakChar
  1 field TEXTMETRIC.Italic
  1 field TEXTMETRIC.Underlined
  1 field TEXTMETRIC.StruckOut
  1 field TEXTMETRIC.PitchAndFamily
  1 field TEXTMETRIC.CharSet
CONSTANT TEXTMETRIC


 0
  4 field RECT.left
  4 field RECT.top
  4 field RECT.right
  4 field RECT.bottom
CONSTANT /RECT

  0
  4 a-field WNDCLASS.style
  8 a-field WNDCLASS.lpfnwndproc
  4 a-field WNDCLASS.cbclsextra
  4 a-field WNDCLASS.cbwndextra
  8 a-field WNDCLASS.hinstance
  8 a-field WNDCLASS.hicon
  8 a-field WNDCLASS.hcursor
  8 a-field WNDCLASS.hbrbackground
  8 a-field WNDCLASS.lpszmenuname
  8 a-field WNDCLASS.lpszclassname
CONSTANT /WNDCLASS-

32 constant LF_FACESIZE

0
  4       a-field cf2.cbSize
  4      a-field cf2.dwMask
  4      a-field cf2.dwEffects
  4       a-field cf2.yHeight
  4       a-field cf2.yOffset
  4   a-field cf2.crTextColor
  1       a-field cf2.bCharSet
  1       a-field cf2.bPitchAndFamily
  LF_FACESIZE   field cf2.szFaceName
  2       a-field cf2.wWeight
  2       a-field cf2.sSpacing
  4   a-field cf2.crBackColor
  4      a-field cf2.lcid
  4      a-field cf2.dwCookie
  2       a-field cf2.sStyle
  2       a-field cf2.wKerning
  1       a-field cf2.bUnderlineType
  1       a-field cf2.bAnimation
  1       a-field cf2.bRevAuthor
  1       a-field cf2.bUnderlineColor
 CONSTANT  /charformat2
