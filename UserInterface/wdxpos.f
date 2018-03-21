c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXPOS ( DEVICE, WID, XPOS, YPOS )
c       --------------------------------------------------
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
c	Moves a window to the given position.  Checks that the window
c       will fit on screen, and, if necessary, modifies the position
c       (returning the new position through the arguments) so that
c       it does.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxpos ( device, wid, xpos, ypos )
c     ==================================================
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
c     Moved location
      integer newx, newy
c
c     Check window will fit
c
      newx = xpos - (winxsi(wid) / 2) * winhor(wid)
      newy = ypos - (winysi(wid) / 2) * winver(wid)
c
      if ( newx .lt. 0 ) newx = 0
      if ( newy .lt. 0 ) newy = 0
c
      if ( newx + winxsi(wid) - 1 .gt.
     +     devxsi(device) - devxmi(device) - 1  ) then
         newx = devxsi(device) - winxsi(wid) + 1
      endif
      if ( newy + winysi(wid) - 1 .gt.
     +     devysi(device) - devymi(device) - 1  ) then
         newy = devysi(device) - winysi(wid) + 1
      endif
c
c     Now check position is still on the device
c
      if ( newx .ge. devxmi(device) .and.
     +     newy .ge. devymi(device) ) then
         newx = newx + (winxsi(wid) / 2) * winhor(wid)
         newy = newy + (winysi(wid) / 2) * winver(wid)
         xpos = newx
         ypos = newy
         status = .FALSE.
      else
c
c        Error - can't move window there
c
         uiferr = MOVWIN
         status = .TRUE.
      endif
c
      wdxpos = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
