c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINHID ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window
c                                is located.
c
c       integer wid : INPUT - the identifier of the window to hide.
c
c	Hides a window off the screen.  Calls the appropriate function
c       to hide the given window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function winhid ( device, wid )
c     =======================================
c
      integer device
      integer wid
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
c     Hides a window on host device
      logical whohid
c
c     Hides a window on framestore device
      logical wfshid
c
      if ( device .eq. HOST ) then
         status = whohid ( device, wid )
      else
         status = wfshid ( device, wid )
      endif
c
      winhid = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
