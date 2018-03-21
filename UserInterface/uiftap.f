c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTAP ( EID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     to which a string is to be appended.
c
c       character*(*) string : INPUT - the string to be appended.
c
c       integer length : INPUT - the length of the string.
c
c       Appends the given string to the contents of the given textfield.
c       If the new contents would be longer than the textfield length,
c       then the contents are truncated to fit.  If the panel owning
c       the textfield is showing, then the textfield is redrawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftap ( eid, string, length )
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
      integer i, j
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
c           OK, so append the text on.
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
                  do 10 i = 1, len
                     j = ind + texcle(eid) + i - 1
                     cstore(j:j) = string(i:i)
10                continue
                  start = texcle(eid) + 1
                  texcle(eid) = texcle(eid) + len
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
      uiftap = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
