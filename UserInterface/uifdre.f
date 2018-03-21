c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFDRE ( DEVICE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - The whose cursor is to be restored.
c
c       Restores the cursor position of the specified device.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifdre ( device )
c     ==================================
c
      integer device
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
c     Cursor position
      integer xpos, ypos
c
c     CALLED FUNCTIONS:
c
c     Pops a cursor position off the stack
      logical uixcpo
c
c     Scans at a position for elements
      logical uixisd
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Pop a cursor position
c
         status = uixcpo ( device, xpos, ypos )
         if ( .not. status ) then
            call devcmo ( device, xpos, ypos )
c
c           Set the cursor position to where it is...
c
            curxpo(device) = xpos
            curypo(device) = ypos
c
c           And scan at the new position
c
            status = uixisd ( device, xpos, ypos )
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifdre = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
