c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WFSCOL ( DEVICE, WID, FGC, BGC )
c       -------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window whose
c                        colours are to be set is located.
c
c       integer wid : INPUT - the identifier of the window whose colours
c                     are to be changed
c
c       integer fgc, bgc : INPUT - the new foreground and background
c                          colours for the window
c
c	Sets the foreground and background colours for a window on the
c       framestore screen.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wfscol ( device, wid, fgc, bgc )
c     =================================================
c
      integer device
      integer wid
      integer fgc, bgc
c
c     CALLED FUNCTIONS:
c
c     Sets the colour of a dumb window
      logical wdxcol
c
      wfscol = wdxcol ( ) !device, wid, fgc, bgc )
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
