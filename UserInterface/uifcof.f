c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCOF ( EID, XPOS, YPOS )
c       -------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell to be
c                     positioned
c
c       integer xpos, ypos : INPUT - the offset within the menu at
c                            which the cell is to be positioned
c
c       Sets the position of the given cell to the given x, y offset
c       within its owning menu element.  The position given is the
c       positioning point of the cell, in device coordinates relative to
c       the top left of the menu.  If the panel owning the menu is
c       showing, an error is generated, since the position of cells
c       within a showing menu cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcof ( eid, xpos, ypos )
c     ===========================================
c
      integer eid
      integer xpos, ypos
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
c              Set the offset
c
               celxof(eid) = xpos
               celyof(eid) = ypos
c
c              Ensure that row-coloumn positioning is not allowed
c
               celrow(eid) = -1
               celcol(eid) = -1
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
      uifcof = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
