c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFDAC ( DEVICE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - The device(s) with which interaction
c                        is to be allowed.
c
c       Sets the device with which interaction is to be allowed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifdac ( device )
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check the device is valid
c
         if ( device .eq. HOST .or. device .eq. FSTORE
     +        .or. device .eq. BOTH ) then
c
c           Save the device
c
            actdev = device
            status = .FALSE.
         else
c
c           Error - bad device
c
            uiferr = BADDEV
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
      uifdac = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
