C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMOP ( DEVICE, XPOS, YPOS )
C       ----------------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the mouse position
C                        is to be set.
C
C       integer xpos, ypos : INPUT - the position to which mouse is to
C                            be set.
C
C       Sets the mouse position.  Resets the panel and element over
C       which the mouse is currently located, on the given device.
C       Sets application variables UIX, UIY, PNO and ENO.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmop ( device, xpos, ypos )
C     ==============================================
C
      integer device
      integer xpos, ypos
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     CALLED FUNCTIONS:
C
C     Sets application variable value
C
      logical appack
C
C     Scans for elements at a position
C
      logical uixisd
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check the device is valid
C
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
C
C           Make sure the position is on the device, and adjust
C           if needed
C
C            if ( xpos .lt. devxmi(device) ) xpos = devxmi(device)
C            if ( xpos .gt. devxmi(device) + devxsi(device) - 1 )
C     +         xpos = devxmi(device) + devxsi(device) - 1
C            if ( ypos .lt. devymi(device) ) ypos = devymi(device)
C            if ( ypos .gt. devymi(device) + devysi(device) - 1 )
C     +         ypos = devymi(device) + devysi(device) - 1
C
            if ( xpos .lt. 0 ) xpos = 0
            if ( xpos .gt. devxsi(device) - 1 ) xpos = devxsi(device)-1
            if ( ypos .lt. 0 ) ypos = 0
            if ( ypos .gt. devysi(device) - 1 ) ypos = devysi(device)-1
C
C           Save the new position, set the cursor to it, and
C           rescan
C
            curxpo(device) = xpos
            curypo(device) = ypos
            call devcmo ( device, xpos, ypos )
            status = uixisd ( device, xpos, ypos )
            if ( .not. status ) then
C
C              OK, set the variables.
C
               status = appack ( -1985, real(xpos) )
               if ( .not. status ) then
                  status = appack ( -1986, real(ypos) )
                  if ( .not. status ) then
                     status = appack ( 26175, real(pidl(device)))
                     if ( .not. status ) then
                        status = appack ( 8575,
     +                                    real(eidl(device)))
                     endif
                  endif
               endif
C
               if ( status ) then
C
C                 Error - couldn't set variable value
C
                  uiferr = VARSET
               endif
            endif
         else
C
C           Error - invalid device
C
            uiferr = BADDEV
            status = .TRUE.
         endif
      else
C
C        Error - system not initialised
C
         uiferr = NOTINI
         status = .TRUE.
      endif
C
      uifmop = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
