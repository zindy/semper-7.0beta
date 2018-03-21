c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCRO ( EID, ROW )
c       ------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell to be
c                             positioned
c
c       integer row : INPUT - the row number at which the cell is to be
c                     positioned
c
c       Sets the position of the given cell to the given row number
c       within its owning menu element.  If the panel owning the menu
c       is showing, an error is generated, since the position of cells
c       within a showing menu cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcro ( eid, row )
c     ====================================
c
      integer eid
      integer row
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
c     Recalculates the size of / redisplays a menu
      logical uixcmp
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check the cell id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL .and.
     +        celnam(eid) .ge. 0 ) then
c
c           Check panel is not showing
c
            if ( .not. paniss(celopa(eid)) ) then
c
c              Set the row
c
               celrow(eid) = row
c
c              And if the column is not set, set it to a default
c              value
c
               if ( celcol(eid) .eq. -1 ) then
                  celcol(eid) = 1
               endif
c
c              Ensure that offset positioning is not allowed
c
               celxof(eid) = -1
               celyof(eid) = -1
c
c              And resize the menu if the cell is on one
c
               if ( celome(eid) .ne. 0 ) then
                  status = uixcmp ( eid, celome(eid), celopa(eid) )
               else
                  status = .FALSE.
               endif
            else
c
c              Error - panel is showing
c
               uiferr = PASHOW
               status = .TRUE.
            endif
         else
c
c           Error - bad cell id
c
            uiferr = BADCEL
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
      uifcro = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
