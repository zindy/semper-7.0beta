c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTCP ( EID, MODE, POS )
c       ------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose cursor is to be moved.
c
c       integer mode : INPUT - the cursor move mode.
c
c       integer pos : INPUT - the position to move the cursor to.
c
c       Moves the cursor in a textfield, depending on the move mode set.
c       If mode is LEFT, the cursor is moved left by pos character
c       positions, and similarly for RIGHT.  If mode is ABSOL, then
c       pos is treated as an absolute position to which to move the
c       cursor.  Checks are made to ensure that the cursor is not moved
c       outside the field: an attempt to do so does not generate an
c       error.  If the panel owning the textfield is showing, then the
c       textfield is redrawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftcp ( eid, mode, pos )
c     ==========================================
c
      integer eid, mode, pos
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
c     New cursor position
      integer newpos
c
c     CALLED FUNCTIONS:
c
c     Moves textfield cursor
      logical uixxmc
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX
     +        .and. texnam(eid) .ge. 0 ) then
c
c           Branch on the move mode
c
            newpos = texcpo(eid)
            if ( mode .eq. LEFT ) then
c
c              Move left by required amount
c
               newpos = texcpo(eid) - pos
               if ( newpos .lt. 1 ) newpos = 1
            else if ( mode .eq. RIGHT ) then
c
c              Move right by required amount
c
               newpos = texcpo(eid) + pos
               if ( newpos .gt. texlen(eid) ) newpos = texlen(eid)
            else if ( mode .eq. ABSOL ) then
c
c              Move to absolute position
c
               newpos = pos
               if ( newpos .lt. 1 ) newpos = 1
               if ( newpos .gt. texlen(eid) ) newpos = texlen(eid)
            endif
c
c           Check if panel is showing - if so, move the cursor
c
            if ( paniss(texopa(eid)) ) then
               if ( newpos .ne. texcpo(eid) ) then
                  status = uixxmc ( eid, newpos )
               else
                  status = .FALSE.
               endif
            else
               status = .FALSE.
            endif
c
c           Save the new cursor position
c
            texcpo(eid) = newpos
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
      uiftcp = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
