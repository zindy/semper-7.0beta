c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXMSI ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the menu whose size
c                     is to be adjusted
c
c       Adjusts the size of menu element with identifier EID to
c       take account of its contents and name, bearing in mind any
c       explicit size allocated to it.
c
c       Function returns FALSE if menu still fits its panel,
c       otherwise TRUE
c
c----------------------------------------------------------------------
c
      logical function uixmsi ( eid )
c     ===============================
c
      integer eid
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Function return status
      logical status
c
c     New size of menu
      integer xsize, ysize
c
c     Position of menu when positioning parameters taken into account
      integer xpos, ypos
c
c     Id of panel owning menu
      integer pid
c
c     Loop counter
      integer i
c
c     CALLED FUNCTIONS:
c
c     Gets the size of a fixed menu
      logical uixxfi
c
c     Gets the size of a pulldown menu
      logical uixxpu
c
c     Gets the size of a popup menu
      logical uixxpo
c
c     Split on the type of menu
c
      if ( mentyp(eid) .eq. FIXED ) then
         status = uixxfi ( eid, xsize, ysize )
      else if ( mentyp(eid) .eq. PULLDN ) then
         status = uixxpu ( eid, xsize, ysize )
      else if ( mentyp(eid) .eq. POPUP ) then
         xsize = 0
         ysize = 0
         status = uixxpo ( eid )
      endif
c
c     Now check to see if menu was explicitly sized, and adjust if
c     we are trying to make it too small.
c
      if ( .not. status ) then
         if ( .not. menexs(eid) ) then
            menxsi(eid) = xsize
            menysi(eid) = ysize
         else
            if ( xsize .gt. menesx(eid) ) then
               menxsi(eid) = xsize
            else
               menxsi(eid) = menesx(eid)
            endif
            if ( ysize .gt. menesy(eid) ) then
               menysi(eid) = ysize
            else
               menysi(eid) = menesy(eid)
            endif
         endif
c
c        If panel owning the menu is NOT autosizing, check
c        that the menu fits on it!.  Must calculate menu position
c        when its positioning parameters are applied first.  Only sane
c        for pulldown and fixed menus
c
         if ( mentyp(eid) .eq. FIXED .or. mentyp(eid) .eq. PULLDN ) then
            pid = menopa(eid)
            if ( .not. panaut(pid) ) then
               status = .TRUE.
               xpos = menxpo(eid) - (menxsi(eid)/2) * menhor(eid)
               if ( xpos .gt. 0 ) then
                  ypos = menypo(eid) - (menysi(eid)/2) * menver(eid)
                  if ( ypos .gt. 0 ) then
                     if ( xpos + xsize - 1 .lt. panxsi(pid) ) then
                        if ( ypos + ysize - 1 .lt. panysi(pid) ) then
                           status = .FALSE.
                        endif
                     endif
                  endif
               endif
            endif
         endif
      endif
c
      if ( status ) then
c
c        Error - bad position or size
c
         uiferr = BADEPO
      else
c
c        Now, for fixed menus only, we need to move the cells to take
c        account of the menu positioning parameters
c
         if ( mentyp(eid) .eq. FIXED ) then
            do 1 i = MINCEL, MAXCEL
               if ( celnam(i) .ge. 0 ) then
                  if ( celome(i) .eq. eid ) then
                     celxpo(i) = celxpo(i) - menxsi(eid)/2 * menhor(eid)
                     celypo(i) = celypo(i) - menysi(eid)/2 * menver(eid)
                  endif
               endif
1           continue
         endif
      endif
c
      uixmsi = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixxfi ( eid, xsize, ysize )
c     =============================================
c
      integer eid, xsize, ysize
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
c     Length of name
      integer namlen
c
c     Position of cell
      integer xpos, ypos
c
c     Max. and min. rows and columns used
      integer maxrow, minrow, maxcol, mincol
c
c     Flag indicating if row-column positioning to be done
      logical rowcol
c
c     Maxwimum width and depth of a row and a column
      integer maxwid, maxdep
c
c     Column depth, and deepest column found
      integer coldep, deepco
c
c     Position of bottom left of menu cell
      integer xopos, yopos
c
c     Loop counters
      integer i, j, k
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Gets string length
      integer USTLEN
c
c     Find menu size from length and name
c
      xsize = 0
      ysize = 0
      namlen = 0
      status = .FALSE.
      if ( mennam(eid) .gt. 0 ) then
         status = pindex ( mennam(eid), ind )
         namlen = USTLEN ( cstore(ind:) )
         xsize = namlen
      endif
c
c     Zoom through the cells working out the max. row and column
c
      maxrow = -1
      maxcol = -1
      minrow = 32767
      mincol = 32767
      rowcol = .FALSE.
      do 1 i = MINCEL, MAXCEL
         if ( celnam(i) .ge. 0 ) then
            if ( celome(i) .eq. eid ) then
               if ( celrow(i) .ne. -1 ) then
                  rowcol = .TRUE.
                  if ( celrow(i) .gt. maxrow ) maxrow = celrow(i)
                  if ( celcol(i) .gt. maxcol ) maxcol = celcol(i)
                  if ( celrow(i) .lt. minrow ) minrow = celrow(i)
                  if ( celcol(i) .lt. mincol ) mincol = celcol(i)
               endif
            endif
         endif
1     continue
c
c     Adjust menu size for extents of its cells.  Do the cell
c     positioning by row-column
c
      if ( rowcol ) then
         xsize = 0
         ysize = 0
         deepco = 0
         xpos = menxpo(eid) + 1
         do 2 i = mincol, maxcol
c
c           Find the maximum width of all cells in this column
c
            maxwid = 0
            do 3 k = MINCEL, MAXCEL
               if ( celnam(k) .ge. 0 ) then
                  if ( celome(k) .eq. eid ) then
                     if ( celcol(k) .eq. i ) then
                        if ( celxsi(k) .gt. maxwid ) maxwid = celxsi(k)
                     endif
                  endif
               endif
3           continue
            if ( maxwid .eq. 0 ) goto 2
c
c           Now do all cells in the column
c
            ypos = menypo(eid) + 1
            coldep = 0
c            do 4 j = minrow, maxrow
            do 4 j = 1, maxrow
c
c              Find the maximum depth of all cells in this row
c
               maxdep = 1
               do 5 k = MINCEL, MAXCEL
                  if ( celnam(k) .ge. 0 ) then
                     if ( celome(k) .eq. eid ) then
                        if ( celrow(k) .eq. j ) then
                           if ( celysi(k) .gt. maxdep )
     +                          maxdep = celysi(k)
                        endif
                     endif
                  endif
5              continue
c
c              Now position the cells in the column
c
               if ( maxdep .gt. 0 ) then
                  do 6 k = MINCEL, MAXCEL
                     if ( celnam(k) .ge. 0 ) then
                        if ( celome(k) .eq. eid ) then
                           if ( celcol(k) .eq. i ) then
                              if ( celrow(k) .eq. j ) then
c
c                                Found a cell, position it
c
                                 if ( celhor(k) .eq. LEFT ) then
                                    celxpo(k) = xpos
                                 else if ( celhor(k) .eq. CENTRE ) then
                                    celxpo(k) = xpos + maxwid / 2 -
     +                                          celxsi(k) / 2
                                 else if ( celhor(k) .eq. RIGHT ) then
                                    celxpo(k) = xpos + maxwid -
     +                                          celxsi(k)
                                 endif
                                 if ( celver(k) .eq. TOP ) then
                                    celypo(k) = ypos
                                 else if ( celver(k) .eq. CENTRE ) then
                                    celypo(k) = ypos + maxdep / 2 -
     +                                          celysi(k) / 2
                                 else if ( celver(k) .eq. BOTTOM ) then
                                    celypo(k) = ypos + maxdep -
     +                                          celysi(k)
                                 endif
                              endif
                           endif
                        endif
                     endif
6                 continue
               endif
               ypos = ypos + maxdep
               coldep = coldep + maxdep
               if ( coldep .gt. deepco ) deepco = coldep
4           continue
            xpos = xpos + maxwid + 1
            xsize = xsize + maxwid + 1
2        continue
         if ( xsize .lt. namlen + 2 ) xsize = namlen + 2
         ysize = deepco
      else
c
c        Do the positioning by offset
c
         do 7 i = MINCEL, MAXCEL
            if ( celnam(i) .ge. 0 ) then
               if ( celome(i) .eq. eid ) then
c
c                 Position by offset
c
                  if ( celxof(i) .ne. -1 ) then
                     xpos = celxof(i) + menxpo(eid)
                     ypos = celyof(i) + menxpo(eid)
                     xpos = xpos - (celxsi(i)/2) * celhor(i)
                     ypos = ypos - (celysi(i)/2) * celver(i)
                     xopos = xpos + celxsi(i) - 1
                     yopos = ypos + celysi(i) - 1
                     celxpo(i) = xpos
                     celypo(i) = ypos
                     if ( xopos .gt. menxpo(eid) + xsize - 1 ) then
                        xsize = xopos - menxpo(eid) + 1
                     endif
                     if ( yopos .gt. menypo(eid) + ysize - 1 ) then
                        ysize = yopos - menypo(eid) + 1
                     endif
                  endif
               endif
            endif
7        continue
      endif
      xsize = xsize + 2
      ysize = ysize + 2
c
      uixxfi = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixxpu ( eid, xsize, ysize )
c     =============================================
c
      integer eid, xsize, ysize
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
c     Length of name
      integer namlen
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Gets string length
      integer USTLEN
c
c     Positions cells on menu panel
      logical uixxpo
c
c     Find menu size from length and name
c
      xsize = 0
      ysize = 0
      status = .FALSE.
      if ( mennam(eid) .gt. 0 ) then
         status = pindex ( mennam(eid), ind )
         namlen = USTLEN ( cstore(ind:) )
         xsize = namlen
         ysize = 1
      endif
c
c     Now position the cells on the menu panel
c
      if ( .not. status ) then
         status = uixxpo ( eid )
      endif
c
      uixxpu = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixxpo ( eid )
c     ===============================
c
      integer eid
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Function return status
      logical status
c
c     Position of cell
      integer xpos, ypos
c
c     Max. and min. rows and columns used
      integer maxrow, minrow, maxcol, mincol
c
c     Flag indicating if row-column positioning to be done
      logical rowcol
c
c     Maxwimum width and depth of a row and a column
      integer maxwid, maxdep
c
c     Column depth, and deepest column found
      integer coldep, deepco
c
c     Identifier of menu panel
      integer mpanel
c
c     Loop counters
      integer i, j, k
c
c     CALLED FUNCTIONS:
c
c     Position the cells on the menu panel
c     Zoom through the cells working out the max. row and column
c
      status = .FALSE.
      maxrow = -1
      maxcol = -1
      minrow = 32767
      mincol = 32767
      rowcol = .FALSE.
      mpanel = menpan(eid)
      if ( mpanel .ne. -1 ) then
         do 1 i = MINCEL, MAXCEL
            if ( celnam(i) .ge. 0 ) then
               if ( celopa(i) .eq. mpanel ) then
                  if ( celrow(i) .ne. -1 ) then
                     rowcol = .TRUE.
                     if ( celrow(i) .gt. maxrow ) maxrow = celrow(i)
                     if ( celcol(i) .gt. maxcol ) maxcol = celcol(i)
                     if ( celrow(i) .lt. minrow ) minrow = celrow(i)
                     if ( celcol(i) .lt. mincol ) mincol = celcol(i)
                  endif
               endif
            endif
1        continue
c
c        Adjust menu size for extents of its cells.  Do the cell
c        positioning by row-column
c
         if ( rowcol ) then
            deepco = 0
            xpos = 1
            do 2 i = 1, maxcol
c
c              Find the maximum width of all cells in this column
c
               maxwid = 0
               do 3 k = MINCEL, MAXCEL
                  if ( celnam(k) .ge. 0 ) then
                     if ( celopa(k) .eq. mpanel ) then
                        if ( celcol(k) .eq. i ) then
                           if ( celxsi(k) .gt. maxwid )
     +                        maxwid = celxsi(k)
                        endif
                     endif
                  endif
3              continue
               if ( maxwid .eq. 0 ) goto 2
c
c              Now do all cells in the column
c
               ypos = 1
               coldep = 0
               do 4 j = 1, maxrow
c
c                 Find the maximum depth of all cells in this row
c
                  maxdep = 1
                  do 5 k = MINCEL, MAXCEL
                     if ( celnam(k) .ge. 0 ) then
                        if ( celopa(k) .eq. mpanel ) then
                           if ( celrow(k) .eq. j ) then
                              if ( celysi(k) .gt. maxdep )
     +                             maxdep = celysi(k)
                           endif
                        endif
                     endif
5                 continue
c
c                 Now position the cells in the column
c
                  if ( maxdep .gt. 0 ) then
                     do 6 k = MINCEL, MAXCEL
                        if ( celnam(k) .ge. 0 ) then
                           if ( celopa(k) .eq. mpanel ) then
                              if ( celcol(k) .eq. i ) then
                                 if ( celrow(k) .eq. j ) then
c
c                                   Note we dont need to take account
c                                   of the cell positioning parameters
c                                   as this will be done when the panel
c                                   is shown
c
                                    celxpo(k) = xpos
                                    celypo(k) = ypos
                                 endif
                              endif
                           endif
                        endif
6                    continue
                  endif
                  ypos = ypos + maxdep
                  coldep = coldep + maxdep
                  if ( coldep .gt. deepco ) deepco = coldep
4              continue
               xpos = xpos + maxwid + 1
2           continue
         else
c
c           Do the positioning by offset
c
            do 7 i = MINCEL, MAXCEL
               if ( celnam(i) .ge. 0 ) then
                  if ( celopa(i) .eq. mpanel ) then
c
c                    Position by offset
c
                     if ( celxof(i) .ne. -1 ) then
c                       xpos = celxof(i) + menxpo(eid)
c                       ypos = celyof(i) + menxpo(eid)
      xpos = celxof(i)
      ypos = celyof(i)
                        xpos = xpos - (celxsi(i)/2) * celhor(i)
                        ypos = ypos - (celysi(i)/2) * celver(i)
                        celxpo(i) = xpos
                        celypo(i) = ypos
                     endif
                  endif
               endif
7           continue
         endif
      endif
c
      uixxpo = status
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
