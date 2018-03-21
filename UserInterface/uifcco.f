c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCCO ( EID, COLUMN )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell to be
c                             positioned
c
c       integer column : INPUT - the column number at which the cell is
c                                to be positioned
c
c       Sets the position of the given cell to the given column number
c       within its menu element.  If the panel owning the menu is
c       showing, an error is generated, since the position of cells
c       within a showing menu cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcco ( eid, column )
c     =======================================
c
      integer eid
      integer column
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
c              Set the column
c
               celcol(eid) = column
c
c              And if the row is not set, set it to a default
c              value
c
               if ( celrow(eid) .eq. -1 ) then
                  celrow(eid) = 1
               endif
c
c              Ensure that offset position is not allowed
c
               celxof(eid) = -1
               celyof(eid) = -1
c
c              And resize the menu owning the cell if it is on one
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
      uifcco = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
