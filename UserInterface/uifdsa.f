c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFDSA ( DEVICE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - The whose cursor is to be saved.
c
c       Saves the cursor position of the specified device.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifdsa ( device )
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
c     Pushes cursor position on the stack
      logical uixcpu
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Get the cursor position
c
         call devcqu ( device, xpos, ypos )
c
c        Push the cursor position
c
         status = uixcpu ( device, xpos, ypos )
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifdsa = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
