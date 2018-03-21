c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTWP ( EID, WPON )
c       -------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     which is to be set write protected.
c
c       logical wpon : INPUT - 'make it write protected' flag.
c
c       Declares the given textfield to be write protected or not,
c       depending on the value of WPON.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftwp ( eid, wpon )
c     =====================================
c
      integer eid
      logical wpon
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
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
            if ( texnam(eid) .ge. 0 ) then
c
c              Set the write protection state
c
               texwp(eid) = wpon
               status = .FALSE.
            else
c
c              Error - bad textfield id
c
               uiferr = BADTEX
               status = .TRUE.
            endif
         else
c
c           Error - bad textfield id
c
            uiferr = BADTEX
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
c     All done
c
      uiftwp = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
