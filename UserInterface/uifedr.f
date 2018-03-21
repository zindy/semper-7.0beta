c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFEDR ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - Identifier of the element to be drawn
c
c       Draws the requested element, which may be of any type.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifedr ( eid )
c     ===============================
c
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
c     CALLED FUNCTIONS:
c
c     Draws a cell element
      logical uifcdr
c
c     Draws a textfield element
      logical uiftdr
c
c     Draws a menu element
      logical uifmdr
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
            if ( elenam(eid) .ge. 0 ) then
c
c              Draw the correct element
c
               if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
c
c                 Only draw cell is it is NOT on a fixed menu
c
                  if ( celome(eid) .le. 0 ) then
                     status = uifcdr ( eid )
                  else
                     status = .FALSE.
                  endif
               else if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
                  status = uiftdr ( eid )
               else if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
                  status = uifmdr ( eid )
               else
c
c                 Unknown element type
c
                  uiferr = BADELE
                  status = .TRUE.
               endif
            else
c
c              Error - invalid element id
c
               uiferr = BADELE
               status = .TRUE.
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
      uifedr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
