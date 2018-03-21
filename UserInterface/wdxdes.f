c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXDES ( DEVICE, WID )
c       --------------------------------------
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
      logical function wdxdes ( device, wid )
c     ======================================
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
c     Hides a window
      logical winhid
c
c     Hide window if it is showing
c
      status = .FALSE.
      if ( winiss(wid) ) then
         if ( winhid ( device, wid ) ) then
            uiferr = DESWIN
            status = .TRUE.
         endif
      endif
c
      wdxdes = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
