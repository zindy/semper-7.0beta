c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WFSPOS ( DEVICE, WID, XPOS, YPOS )
c       ---------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides.
c
c       integer wid : INPUT - the identifier of the window to position.
c
c       integer xpos, ypos : INPUT/OUTPUT - the location at which to
c                                           position the window.
c
c	Moves a window to the given position.  Checks that the window
c       will fit on screen, and, if necessary, modifies the position
c       (returning the new position through the arguments) so that it
c       does.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wfspos ( device, wid, xpos, ypos )
c     ===================================================
c
      integer device
      integer wid
      integer xpos, ypos
c
c     CALLED FUNCTIONS:
c
c     Moves a window on the framestore screen
      logical wdxpos
c
      wfspos = wdxpos ( device, wid, xpos, ypos )
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
