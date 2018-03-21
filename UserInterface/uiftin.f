c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTIN ( EID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     into which a string is to be inserted.
c
c       character*(*) string : INPUT - the string to be inserted.
c
c       integer length : INPUT - the length of the string.
c
c       Inserts the given string into the contents of the given
c       textfield at the current cursor position, which is moved to
c       take account of the insert.  If the new contents would be longer
c       than the textfield length, then the contents are truncated to
c       fit.  If the panel owning the textfield is showing, then the
c       textfield is redrawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftin ( eid, string, length )
c     ===============================================
c
      integer eid
      character*(*) string
      integer length
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
c     Amount of space left in field
      integer sleft
c
c     Length of string we are really appending
      integer len
c
c     Physical index of store
      integer ind
c
c     Loop counters
      integer i, j, k
c
c     Number of characters to move
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
c     Finds the currently filled length of a textfield
      integer uixxle
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
c           OK, so insert the text.
c
            sleft = texlen(eid) - texcle(eid)
            if ( length .gt. sleft ) then
               len = sleft
            else
               len = length
            endif
            if ( len .gt. 0 ) then
               status = pindex ( texcon(eid), ind )
               if ( .not. status ) then
c
c                 Shuffle up contents to make space for the new
c                 characters.  Only needed if cursor in filled part
c                 of field
c
                  if ( texcpo(eid) .le. texcle(eid) ) then
                     j = ind + texcle(eid) - 1
                     k = j + len
                     nomove = texcle(eid) - texcpo(eid) + 1
                     do 10 i = 1, nomove
                        cstore(k:k) = cstore(j:j)
                        k = k - 1
                        j = j - 1
   10                continue
                  endif
c
c                 Now do the insert
c
                  k = ind + texcpo(eid) - 1
                  do 20 i = 1, len
                     cstore(k:k) = string(i:i)
                     k = k + 1
   20             continue
                  start = texcpo(eid)
                  texcle(eid) = uixxle ( eid, cstore(ind:) )
                  texcpo(eid) = texcpo(eid) + len
                  if ( texcpo(eid) .gt. texlen(eid) ) then
                     texcpo(eid) = texlen(eid)
                  endif
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
      uiftin = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      integer function uixxle ( eid, cons )
c     =====================================
c
      integer eid
      character*(*) cons
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     LOCAL VARIABLES:
c
c     Loop counter
      integer i
c
c     Field length
      integer length
c
c     Initialise length to zero in case field is all blanks
c
      length = 0
c
c     Walk backwards down the field, looking for non-space characters
c
      do 10 i = texlen(eid), 1, -1
         if ( cons(i:i) .ne. ' ' ) then
            length = i
            goto 20
         endif
   10 continue
   20 continue
c
      uixxle = length
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
