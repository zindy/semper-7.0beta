c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WFSSHO ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the device resides
c
c       integer wid : INPUT - the identifier of the window to show.
c
c	Shows a window on screen.  Clears the approriate area of the
c       screen to the window background colour, and draws the window
c       border and window name.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wfssho ( device, wid )
c     =======================================
c
      integer device
      integer wid
c
c     CALLED FUNCTIONS:
c
c     Shows a window on the framestore screen
      logical wdxsho
c
      wfssho = wdxsho ( device, wid )
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
