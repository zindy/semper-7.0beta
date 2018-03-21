C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFDQU ( DEVICE )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - The device whose size is to be queried.
C
C       Sets the size of the requested device into application
C       variables UIX, UIY, and the number of colours supported
C       into NCO.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifdqu ( device )
C     ==================================
C
      integer device
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
C     Sets the value of an application variable
C
      logical appack
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check the device is valid
C
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
C
C           Set the size into the variables
C
            status = appack ( -1985, real (devxsi(device)) )
            if ( .not. status ) then
               status = appack ( -1986, real (devysi(device)) )
               if ( .not. status ) then
                  status = appack ( 22535, real (devnco(device))
     +)
               endif
            endif
         else
C
C           Error - bad device
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
      uifdqu = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
