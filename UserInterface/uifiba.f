c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFIBA ( ENABLE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       logical enable : INPUT - flag indicating whether before and
c                        after actions should be enabled.
c
c       Enables or diables before and after actions, depending on the
c       value of ENABLE.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifiba ( enable )
c     ==================================
c
      logical enable
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
c        Set the flag
c
         uifaac = enable
         status = .FALSE.
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifiba = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
