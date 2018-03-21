c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXCSI ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose size is
c                     to be adjusted
c
c       Adjusts the size of text cell element with identifier EID to
c       take account of its contents and name, bearing in mind any
c       explicit size allocated to it.
c
c       Function returns FALSE if the cell still fits its panel,
c       otherwise TRUE
c
c----------------------------------------------------------------------
c
      logical function uixcsi ( eid )
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
      integer conlen, namlen
c
c     New size cell size
      integer xsize, ysize
c
c     Position of cell after positioning parameters considered
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
c     Find cell size from its contents and highlighting style
c
      if ( celcon(eid) .gt. 0 ) then
         status = pindex ( celcon(eid), ind )
         conlen = USTLEN ( cstore(ind:) )
         xsize = conlen
         if ( celsty(eid) .eq. CHECK .or. celsty(eid) .eq. TICK ) then
            xsize = xsize + 1
         endif
         ysize = 1
      else
         xsize = 0
         ysize = 0
         conlen = 0
      endif
c
c     Adjust sizes if cell is named
c
      if ( celnam(eid) .gt. 0 .or. celbox(eid) ) then
         if ( celnam(eid) .gt. 0 ) then
            status = pindex ( celnam(eid), ind )
            namlen = USTLEN ( cstore(ind:) )
            if ( namlen .gt. conlen ) then
               xsize = namlen + 2
            else
               xsize = conlen + 2
            endif
         else
            xsize = conlen + 2
         endif
         if ( celsty(eid) .eq. CHECK .or. celsty(eid) .eq. TICK ) then
            xsize = xsize + 1
         endif
         ysize = ysize + 2
      endif
c
c     Now check to see if cell was explicitly sized, and adjust if
c     we are trying to make it too small
c
      if ( .not. celexs(eid) ) then
         celxsi(eid) = xsize
         celysi(eid) = ysize
      else
         if ( xsize .gt. celesx(eid) ) then
            celxsi(eid) = xsize
         else
            celxsi(eid) = celesx(eid)
         endif
         if ( ysize .gt. celesy(eid) ) then
            celysi(eid) = ysize
         else
            celysi(eid) = celesy(eid)
         endif
      endif
c
c     Last thing: if panel owning the cell is NOT autosizing, check
c     that the cell fits on it!.  Must calculate cell position when
c     its positioning parameters are applied first.
c
      pid = celopa(eid)
      if ( .not. panaut(pid) ) then
         xpos = celxpo(eid) - (celxsi(eid)/2) * celhor(eid)
         status = .TRUE.
         if ( xpos .gt. 0 ) then
            ypos = celypo(eid) - (celysi(eid)/2) * celver(eid)
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
      uixcsi = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
