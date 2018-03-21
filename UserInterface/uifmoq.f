C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMOQ ( DEVICE )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the mouse position
C                        is to be queried.
C
C       Queries the mouse position, and number of buttons, and the panel
C       and element over which the mouse is currently located, on the
C       given device.  Sets application variables UIX, UIY, NBU, PNO
C       and ENO.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmoq ( device )
C     ==================================
C
      integer device
C
      include 'EVENTS'
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
C     Number of mouse buttons
C
      integer nbuts
C
C     Dummy variable
C
      integer dummy
C
C     CALLED FUNCTIONS:
C
C     Sets application variable value
C
      logical appack
C
C     Gets event queue data
C
      logical eqgetd
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check the device is valid
C
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
C
C           OK, set the variables.
C
               status = appack ( -1985, real(curxpo(device)))
               if ( .not. status ) then
                  status = appack ( -1986, real(curypo(device)))
                  if ( .not. status ) then
C
C                    Get the number of buttons
C
                     status = eqgetd ( MBUT, nbuts, dummy, dummy )
                     if ( .not. status ) then
                        status = appack ( 22501, real(nbuts) )
                        if ( .not. status ) then
                           status = appack ( 26175,
     +                                        real(pidl(device)))
                           if ( .not. status ) then
                              status = appack ( 8575,
     +                                           real(eidl(device)))
                           endif
                        endif
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
      uifmoq = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
