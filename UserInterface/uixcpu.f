c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXCPU ( DEVICE, XPOS, YPOS )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device whose cursor position
c                        is to be saved
c
c       integer xpos, ypos : INPUT - the cursor position
c
c       Saves the cursor position onto the stack for the requested
c       device.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixcpu ( device, xpos, ypos )
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
c     Stack position to write to from
      integer stkpos
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check stack is not full
c
         stkpos = stktop(device) + 1
         if ( stkpos .le. STKSIZ ) then
c
c           Put the position at the stack top
c
            stkxpo(device,stkpos) = xpos
            stkypo(device,stkpos) = ypos
            stktop(device) = stkpos
            status = .FALSE.
         else
c
c           Error - stack is full
c
            uiferr = STKFUL
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
      uixcpu = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
