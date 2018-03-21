c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WFSBOX ( DEVICE, WID, WIDTH, FGC, BGC,
c              STRING, LENGTH, STYLE, POS, XPOS, YPOS, XSIZE, YSIZE )
c       -------------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window into
c                        which box is to be written is located.
c
c       integer wid : INPUT - the window into which box is to be
c                     written.
c
c       integer width : INPUT - the width of the box edge lines (1 for
c                       single line, 2 for double)
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours in which to write the box.
c
c       character*(*) string : INPUT - banner string to write in the
c                              box to edge
c
c       integer length : INPUT - the length of the above string.  If
c                        zero, no banner is written.
c
c       integer style : INPUT - the style (NORMAL, INVERS, UNDERL) in
c                       which to write the text.
c
c       integer pos : INPUT - the banner position in the box top edge
c                     (LEFT, CENTRE or RIGHT)
c
c       integer xpos, ypos : INPUT - the top left position for the box,
c                            window coordinates
c
c       integer xsize, ysize : INPUT - the size of the box, window
c                              coordinates
c
c	Draws a box to the requested window on the framestore device,
c       in the appropriate colours and style.  If requested, a string
c       will be written into the top edge of the box.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wfsbox ( device, wid, width, fgc, bgc,
     +          string, length, style, pos, xpos, ypos, xsize, ysize )
c     ================================================================
c
      integer device, wid, width, fgc, bgc
      character*(*) string
      integer length, style, pos, xpos, ypos, xsize, ysize
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     CALLED FUNCTIONS:
c
c     Writes box into a dumb window window
      logical wdxbox
c
      status = wdxbox ( device, wid, width, fgc, bgc,
     +          string, length, style, pos, xpos, ypos, xsize, ysize )
c
      wfsbox = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
