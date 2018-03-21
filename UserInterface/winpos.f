c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINPOS ( DEVICE, WID, XPOS, YPOS )
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
c	Moves a window to the given position.  Calls the appropriate function
c       to move a window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function winpos ( device, wid, xpos, ypos )
c     ===================================================
c
      integer device
      integer wid
      integer xpos, ypos
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
c     Sets window position on host device
      logical whopos
c
c     Sets window position on framestore device
      logical wfspos
c
      if ( device .eq. HOST ) then
         status = whopos ( device, wid, xpos, ypos )
      else
         status = wfspos ( device, wid, xpos, ypos )
      endif
c
      winpos = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
