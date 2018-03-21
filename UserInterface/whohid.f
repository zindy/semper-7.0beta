c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WHOHID ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window is
c                                located.
c
c       integer wid : INPUT - the identifier of the window to hide.
c
c	Hides a window off the screen.  Adjusts the panel stack to take
c       account of the hide, clears the area of the screen occupied by
c       the window, and repaints all windows over the appropriate extent
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function whohid ( device, wid )
c     =======================================
c
      integer device
      integer wid
c
c     CALLED FUNCTIONS:
c
c     Hides a window on the host screen
      logical wdxhid
c
      whohid = wdxhid ( device, wid )
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
