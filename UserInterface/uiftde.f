c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTDE ( EID, NCHARS )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     from which characters are to be deleted
c
c       integer nchars : INPUT - the number of characters to delete
c
c       Deletes the requested number of characters from the contents of
c       the given textfield to the left of the current cursor position,
c       which is moved to take account of the delete.  If the panel
c       owning the textfield is showing, then the textfield is redrawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftde ( eid, nchars )
c     ========================================
c
      integer eid
      integer nchars
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Physical index of store
      integer ind
c
c     Loop counters
      integer i, j, k
c
c     Number of characters to move/clear
      integer nomove
c
c     Start index of contents we want to redraw from
      integer start
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Draws textfield contents
      logical uixtdc
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
c           OK, so delete the characters
c
            if ( texcpo(eid) - nchars .ge. 1 ) then
               status = pindex ( texcon(eid), ind )
               if ( .not. status ) then
c
c                 Check if cursor is off end of filled part of field
c                 If so only need a cursor move
c
                  if ( texcpo(eid) .le. texcle(eid) + 1 ) then
c
c                    Shuffle up contents to overwrite characters.
c
                     j = ind + texcpo(eid) - 1
                     k = j - nchars
                     nomove = texcle(eid) - texcpo(eid) + 1
                     do 10 i = 1, nomove
                        cstore(k:k) = cstore(j:j)
                        k = k + 1
                        j = j + 1
10                   continue
c
c                    Now add blanks to the end
c
                     k = ind + texcle(eid) - 1
                     do 20 i = 1, nchars
                        cstore(k:k) = ' '
                        k = k - 1
20                   continue
c
                     texcle(eid) = texcle(eid) - nchars
                  endif
                  texcpo(eid) = texcpo(eid) - nchars
                  start = texcpo(eid)
               endif
c
c              Check if panel is showing - if so, redraw the textfield
c
               if ( .not. status ) then
                  if ( paniss(texopa(eid)) ) then
                     status = uixtdc ( eid, start )
                  endif
               endif
            else
               status = .FALSE.
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
      uiftde = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
