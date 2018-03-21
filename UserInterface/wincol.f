c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINCOL ( DEVICE, WID, FGC, BGC )
c       -------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window whose
c                        colours are to be set is located.
c
c       integer wid : INPUT - the identifier of the window whose colours
c                     are to be changed
c
c       integer fgc, bgc : INPUT - the new foreground and background
c                          colours for the window
c
c	Sets the foreground and background colours for a window.
c       Calls the appropriate function to set window colours on the
c       given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wincol ( device, wid, fgc, bgc )
c     =================================================
c
      integer device
      integer wid
      integer fgc, bgc
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
c     Sets window colour on host device
      logical whocol
c
c     Sets window colour on framestore device
      logical wfscol
c
      if ( device .eq. HOST ) then
         status = whocol ( device, wid, fgc, bgc )
      else
         status = wfscol ( device, wid, fgc, bgc )
      endif
c
      wincol = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
