c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WFSDES ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window to be
c                        destroyed is located.
c
c       integer wid : INPUT - the identifier of the window to destroy.
c
c	Destroys a window.  If the window is showing, hides it,
c       otherwise no action.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wfsdes ( device, wid )
c     =======================================
c
      integer device
      integer wid
c
c     CALLED FUNCTIONS:
c
c     Destroys a window on the framestore screen
      logical wdxdes
c
      wfsdes = wdxdes ( device, wid )
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
