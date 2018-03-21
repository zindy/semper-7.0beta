c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXTSI ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the tetxfield whose size
c                     is to be adjusted
c
c       Adjusts the size of textfield element with identifier EID to
c       take account of its contents and name, bearing in mind any
c       explicit size allocated to it.
c
c       Function returns FALSE if textfield still fits its panel,
c       otherwise TRUE
c
c----------------------------------------------------------------------
c
      logical function uixtsi ( eid )
c     ===============================
c
      integer eid
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Function return status
      logical status
c
c     Physical index of store
      integer ind
c
c     Length of contents and name
      integer namlen
c
c     New cell size
      integer xsize, ysize
c
c     Position of textfield after positioning parameters considered
      integer xpos, ypos
c
c     Identifier of owning panel
      integer pid
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Gets string length
      integer USTLEN
c
c     Find textfield size from length and name
c
      xsize = texlen(eid)
      ysize = 1
      if ( texnam(eid) .gt. 0 ) then
         status = pindex ( texnam(eid), ind )
         namlen = USTLEN ( cstore(ind:) )
         xsize = namlen + 2 + xsize
      endif
c
c     Now check to see if textfield was explicitly sized, and adjust if
c     we are trying to make it too small
c
      if ( .not. texexs(eid) ) then
         texxsi(eid) = xsize
         texysi(eid) = ysize
      else
         if ( xsize .gt. texesx(eid) ) then
            texxsi(eid) = xsize
         else
            texxsi(eid) = texesx(eid)
         endif
         if ( ysize .gt. texesy(eid) ) then
            texysi(eid) = ysize
         else
            texysi(eid) = texesy(eid)
         endif
      endif
c
c     Last thing: if panel owning the textfield is NOT autosizing, check
c     that the textfield fits on it!.  Must calculate textfield position
c     when its positioning parameters are applied first.
c
      pid = texopa(eid)
      if ( .not. panaut(pid) ) then
         xpos = texxpo(eid) - (texxsi(eid)/2) * texhor(eid)
         status = .TRUE.
         if ( xpos .gt. 0 ) then
            ypos = texypo(eid) - (texysi(eid)/2) * texver(eid)
            if ( ypos .gt. 0 ) then
               if ( xpos + xsize - 1 .lt. panxsi(pid) ) then
                  if ( ypos + ysize - 1 .lt. panysi(pid) ) then
                     status = .FALSE.
                  endif
               endif
            endif
         endif
      else
         status = .FALSE.
      endif
c
      if ( status ) then
c
c        Error - bad position or size
c
         uiferr = BADEPO
      endif
c
      uixtsi = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
