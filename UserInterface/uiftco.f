c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTCO ( EID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose contents are to be set.
c
c       character*(*) string : INPUT - the string containing the
c                              contents.
c
c       integer length : INPUT - the length of the contents string.
c
c       Replaces the contents of the given textfield with the given
c       string.  The string may be a null string, in which case the
c       contents are cleared (if the textfield is selected, then the
c       text cursor is positioned to the start of the field).  If the
c       new contents would be longer than the textfield length, then
c       the contents are truncated to fit.  If the panel owning the
c       textfield is showing, then the textfield is redrawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftco ( eid, string, length )
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
c     Length of string we are really appending
      integer len
c
c     Physical index of store
      integer ind
c
c     Loop counters
      integer i, j
c
c     Position from which to fill string with spaces
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
c           OK, so replace the contents
c
            if ( length .gt. texlen(eid) ) then
               len = texlen(eid)
            else
               len = length
            endif
            status = pindex ( texcon(eid), ind )
            if ( .not. status ) then
c
c              If length <= 0, clear the field contents
c
               if ( len .gt. 0 ) then
                  do 10 i = 1, len
                     j = ind + i - 1
                     cstore(j:j) = string(i:i)
10                continue
                  texcle(eid) = len
                  start = len + 1
               else
                  texcle(eid) = 0
                  texcpo(eid) = 1
                  start = 1
               endif
c
c              Fill to end of field with spaces
c
               do 20 i = start, texlen(eid)
                  j = ind + i - 1
                  cstore(j:j) = ' '
20             continue
c
c              Check if panel is showing - if so, redraw the textfield
c
               if ( .not. status ) then
                  if ( paniss(texopa(eid)) ) then
                     status = uixtdc ( eid, 1 )
                  endif
               endif
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
      uiftco = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
