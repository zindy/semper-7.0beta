c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINDES ( DEVICE, WID )
c       ---------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window to be
c                        destroyed is located.
c
c       integer wid : INPUT - the identifier of the window to destroy.
c
c	Destroys a window.  Calls the appropriate function to destroy a
c       window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function windes ( device, wid )
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
c     Destroys a window on host device
      logical whodes
c
c     Destroys a window on framestore device
      logical wfsdes
c
      if ( device .eq. HOST ) then
         status = whodes ( device, wid )
      else
         status = wfsdes ( device, wid )
      endif
c
      windes = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
