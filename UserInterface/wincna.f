c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINCNA ( DEVICE, WID, NAME, LENGTH )
c       -----------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides.
c
c       integer wid : INPUT - the identifier of the window to name.
c
c       character*(*) name : INPUT - the name to give to the window.
c
c       integer length : INPUT - then length of the name.
c
c	Changes the name of a window.  Calls the appropriate function to
c       change the name of a window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wincna ( device, wid, name, length )
c     =====================================================
c
      integer device
      integer wid
      character*(*) name
      integer length
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
c     Changes the name of a window on host device
      logical whocna
c
c     Changes the name of a window on framestore device
      logical wfscna
c
      if ( device .eq. HOST ) then
         status = whocna ( device, wid, name, length )
      else
         status = wfscna ( device, wid, name, length )
      endif
c
      wincna = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
