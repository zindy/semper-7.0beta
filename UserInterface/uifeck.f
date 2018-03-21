c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFECK ( TYPE, EID )
c       -------------------------------------
c
c       PARAMETERS:
c
c       integer type : INPUT - the supposed type of an element.
c
c       integer eid : INPUT - the identifier of the element to check.
c
c       Checks that the given element is of the requested type.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifeck ( type, eid )
c     =====================================
c
      integer type
      integer eid
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
c        Check element id is valid
c
         if ( eid .ge. MINELE .and. eid .le. MAXELE
     +        .and. elenam(eid) .ge. 0 ) then
c
c           Branch on type, checking id aginst range for for each
c           element type.
c
            status = .FALSE.
            if ( type .eq. CELL ) then
               if ( eid .lt. MINCEL .or. eid .gt. MAXCEL ) then
                  uiferr = BADCEL
                  status = .TRUE.
               endif
            else if ( type .eq. TEXFLD ) then
               if ( eid .lt. MINTEX .or. eid .gt. MAXTEX ) then
                  uiferr = BADTEX
                  status = .TRUE.
               endif
            else if ( type .eq. MENU ) then
               if ( eid .lt. MINMEN .or. eid .gt. MAXMEN ) then
                  uiferr = BADMEN
                  status = .TRUE.
               endif
            else if ( type .eq. GSIGHT ) then
            else if ( type .eq. SLIDER ) then
            else if ( type .eq. KNOB ) then
            endif
         else
c
c           Error - invalid element id
c
            uiferr = BADELE
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
      uifeck = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
