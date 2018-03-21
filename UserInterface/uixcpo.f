c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXCPO ( DEVICE, XPOS, YPOS )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device whose cursor position
c                        is required
c
c       integer xpos, ypos : OUTPUT - the cursor position
c
c       Returns the cursor position at the top of the stack for
c       the requested device.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixcpo ( device, xpos, ypos )
c     ==============================================
c
      integer device
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
c     Stack position to read from
      integer stkpos
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check stack is not empty
c
         stkpos = stktop(device)
         if ( stkpos .gt. 0 ) then
c
c           Get the position at the stack top
c
            xpos = stkxpo(device,stkpos)
            ypos = stkypo(device,stkpos)
            stktop(device) = stkpos - 1
            status = .FALSE.
         else
c
c           Error - stack is empty
c
            uiferr = STKEMP
            status = .TRUE.
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uixcpo = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
