c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINSHO ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides.
c
c       integer wid : INPUT - the identifier of the window to show.
c
c	Shows a window on screen.  Calls the appropriate function to show
c       a window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function winsho ( device, wid )
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
c     Shows a window on host device
      logical whosho
c
c     Shows a window on framestore device
      logical wfssho
c
      if ( device .eq. HOST ) then
         status = whosho ( device, wid )
      else
         status = wfssho ( device, wid )
      endif
c
      winsho = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
