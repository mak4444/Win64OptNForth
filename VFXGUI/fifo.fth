  0
  CELL field fifo_size
  CELL field fifo_memptr
  CELL field fifo_tail
  CELL field fifo_head
  CELL field fifo_used
 CONSTANT FIFO

: InitialiseFIFO-        \ *FIFO size -- ior
\ *G Initialise a FIFO with a maximum buffer 'size' bytes long.
\ ** the IOR is 0 for success and non-zero for memory allocation failed.
  swap >r
  dup r@ fifo_size !
  CELL+ allocate if
    drop -1
  else
    dup r@ fifo_memptr !
    dup r@ fifo_head !
        r@ fifo_tail !
    0   r@ fifo_used !
    0
  then
  r> drop
;


: FIFO?         \ *FIFO -- n ; Return # bytes used in fifo
\ *G Return the number of storage bytes in use within a fifo.
  fifo_used @
;

: bump-head	\ *fifo -- *fifo ; Move HEAD on one position
  >r
  r@ fifo_head @ 1+     \ newhead --
  r@ fifo_memptr @
  r@ fifo_size @ +      \ newhead last
  over                  \ newhead last newhead
  <= if
    r@ fifo_size @ -
  then
  r@ fifo_head !
  r> drop
;


: bump-tail     \ *fifo -- *fifo ; Move TAIL on one position
  >r
  r@ fifo_tail @ 1+     \ newhead --
  r@ fifo_memptr @
  r@ fifo_size @ +      \ newhead last
  over                  \ newhead last newhead
  <= if
    r@ fifo_size @ -
  then
  r@ fifo_tail !
  r> drop
;

: >FIFO(b)      \ BYTE *FIFO -- ior
( *G Add a byte to the FIFO queue. IOR is 0 for success. )
  dup fifo_used  @
  over fifo_size @
  = if
    2drop -1            \ no room in FIFO
  else
    dup fifo_head @     \ -- byte *fifo *head
    rot swap c!         \ -- *fifo
    dup bump-head
    fifo_used 1 swap +!
    0
  then
;

: FIFO>(b)	\ *FIFO -- byte ior
( *G Remove next byte from a FIFO. IOR is 0 for success. )
  dup fifo_used @ if
    dup fifo_tail @ c@		\ -- *FIFO byte
    over bump-tail
    swap fifo_used -1 swap +!
    0
  else
    -1
  then
;

