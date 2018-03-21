c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCBO ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell to be marked
c                     as having a box around
c
c       Sets the given cell to have a box around it.  (This is done
c       automatically if the cell is named.)
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcbo ( eid )
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check panel is showing
c
         if ( .not. paniss(celopa(eid)) ) then
c
c           Check element id is valid
c
            if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
               if ( celnam(eid) .ge. 0 ) then
c
c                 OK, mark it as having a box.
c
                  celbox(eid) = .TRUE.
                  status = .FALSE.
               else
c
c                 Error - invalid cell id
c
                  uiferr = BADCEL
                  status = .TRUE.
               endif
            else
c
c              Error - invalid cell id
c
               uiferr = BADCEL
               status = .TRUE.
            endif
         else
c
c           Error - panel is showing
c
            uiferr = PASHOW
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
      uifcbo = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
