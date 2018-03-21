c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WHOTEX ( DEVICE,WID,FGC,BGC,STYLE,XPOS,YPOS,
c                                 STRING, LENGTH )
c       ---------------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window into
c                        which text is to be written is located.
c
c       integer wid : INPUT - the window into which text is to be
c                     written.
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours in which to write the text.
c
c       integer style : INPUT - the style (NORMAL, INVERS, UNDERL) in
c                       which to write the text.
c
c       integer xpos, ypos : INPUT - the top left position for the text
c
c       character*(*) string : INPUT - the string to be written.
c
c       integer length : INPUT - the length of the string.
c
c	Writes a string to the requested window, in the appropriate
c       colours and style, on the host device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function whotex ( device, wid, fgc, bgc, style, xpos,
     +                          ypos, string, length )
c     ==========================================================
c
      integer device, wid, fgc, bgc, style
      integer xpos, ypos, length
      character*(*) string
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
c     Writes text on dumb device window
      logical wdxtex
c
      status = wdxtex ( device, wid, fgc, bgc, style, xpos,
     +                     ypos, string, length )
c
      whotex = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
