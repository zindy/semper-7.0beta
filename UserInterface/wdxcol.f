c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXCOL ( DEVICE, WID, FGC, BGC )
c       ------------------------------------------------
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
c	Sets the foreground and background colours for a window. Is a noop for
c       dumb screens.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxcol ( ) !device, wid, fgc, bgc )
c     ================================================
c
!     integer device
!     integer wid
!     integer fgc, bgc
c
      wdxcol = .FALSE.
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
