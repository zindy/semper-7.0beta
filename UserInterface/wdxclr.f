c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION WDXCLR ( DEVICE, WID, COLOUR, XPOS, YPOS,
c                                 XSIZE, YSIZE )
c       ----------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides
c
c       integer wid : INPUT - the identifier of the window to clear.
c
c       integer colour : INPUT - the colour to clear the area to.
c
c       integer xpos, ypos : INPUT - the top left of the area to clear,
c                            in window coordinates.
c
c       integer xsize, ysize : INPUT - the size of the area to clear,
c                              in window coordinates.
c
c       Clears an area of a window on screen.  If the window to be
c       cleared is not obscured, then the area is cleared directly.
c       Otherwise, spcial action is taken to clear the window.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxclr ( device, wid, colour, xpos, ypos,
     +                          xsize, ysize )
c     ==========================================================
c
      integer device, wid, colour, xpos, ypos, xsize, ysize
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
c     Position and size of the area to clear in device space, clipped
c     to clip rectange
      integer dxpos, dypos, dxsize, dysize
c
c     CALLED FUNCTIONS:
c
c     Clears and area of a device
      logical devclr
c
c     Clears an area of an obscured window
      logical wdxcow
c
c     If window is not obscured, can do quick and easy clear
c
      if ( .not. winobs(wid) ) then
c
c        First form an extent to clear, taking into account the
c        clip rectangle set, and convert to device space.
c
         dxpos = xpos + winxpo(wid)
         dypos = ypos + winypo(wid)
         dxsize = xsize
         dysize = ysize
c
         if ( dxpos .lt. clipl ) then
            dxsize = dxsize - (clipl - dxpos)
            dxpos = clipl
         endif
         if ( dypos .lt. clipt ) then
            dysize = dysize - (clipt - dypos)
            dypos = clipt
         endif
         if ( dxpos + dxsize - 1 .gt. clipr ) dxsize = clipr - dxpos + 1
         if ( dypos + dysize - 1 .gt. clipb ) dysize = clipb - dypos + 1
c
c        Check area to clear is in the window
c
         status = .TRUE.
         if ( dxpos .ge. winxpo(wid) ) then
            if ( dypos .ge. winypo(wid) ) then
               if ( dxpos + dxsize - 1 .le.
     +              winxpo(wid) + winxsi(wid) - 1 ) then
                  if ( dypos + dysize - 1 .le.
     +                 winypo(wid) + winysi(wid) - 1 ) then
c
c                    OK, all is kosher
c
                     status = .FALSE.
                  endif
               endif
            endif
         endif
c
         if ( .not. status ) then
c
c           Clear the area to required colour
c
            status = devclr ( device,colour,dxpos,dypos,dxsize,dysize )
            if ( status ) uiferr = CLRWIN
         else
c
c           Error - area to clear not in window
c
            uiferr = CLRWIN
         endif
      else
c
c        Take special action to clear area of obscured window
c
         status = wdxcow ( device, wid, colour, xpos, ypos,
     +                     xsize, ysize )
      endif
c
      wdxclr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION WDXCOW ( DEVICE, WID, COLOUR, XPOS, YPOS,
c                                 XSIZE, YSIZE )
c       ----------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides
c
c       integer wid : INPUT - the identifier of the window to clear.
c
c       integer colour : INPUT - the colour to clear the area to.
c
c       integer xpos, ypos : INPUT - the top left of the area to clear,
c                            in window coordinates.
c
c       integer xsize, ysize : INPUT - the size of the area to clear,
c                              in window coordinates.
c
c       Clears an area of an obscured window on screen.  Calculates
c       the obscured extent of the area to clear, then clears the area
c       around that which is not obscured.  Any unobscured portions
c       of the obscured extent are then cleared by spliting the
c       obscured extent into horizontal strips delineated by the top
c       and bottom of obscuring windows.  Each strip is then split
c       into horizontal segments, each segment delinated by the
c       vertical edges of obscuring windows.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxcow ( device, wid, colour, xpos, ypos,
     +                          xsize, ysize )
c     ==========================================================
c
      integer device, wid, colour, xpos, ypos, xsize, ysize
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
c     Position, opposite corner and size of the area to clear in
c     device space, clipped to clip rectange
      integer dxpos, dypos, doxpos, doypos, dxsize, dysize
c
c     Obscured rectangle corners, device coords.
      integer obsl, obst, obsr, obsb
c
c     Number of obscuring rectangles forming obscured extent
      integer noobs
c
c     Temporary variable
      integer itemp
c
c     Loop counters
      integer i, j
c
c     Edges of the window
      integer winlef, wintop, winrig, winbot
c
c     Left, right, top, bottom and size of obscured area to clear
      integer lclear, rclear, tclear, bclear, sizex, sizey
c
c     Highest top and bototm of windows crossing area of interest
      integer higtop, higbot
c
c     Edges of a segment to clear
      integer ledge, redge
c
c     CALLED FUNCTIONS:
c
c     Checks two rectangles for intersection
      logical uixint
c
c     Clears and area of a device
      logical devclr
c
c     Finds next segment of a strip to clear
      logical wdxfns
c
c     First form an extent to clear, and convert to device space.
c
      dxpos = xpos + winxpo(wid)
      dypos = ypos + winypo(wid)
      dxsize = xsize
      dysize = ysize
c
c     Take account of clipping rectange
c
      if ( dxpos .lt. clipl ) then
         dxsize = dxsize - (clipl - dxpos)
         dxpos = clipl
      endif
      if ( dypos .lt. clipt ) then
         dysize = dysize - (clipt - dypos)
         dypos = clipt
      endif
      if ( dxpos + dxsize - 1 .gt. clipr ) dxsize = clipr - dxpos + 1
      if ( dypos + dysize - 1 .gt. clipb ) dysize = clipb - dypos + 1
c
      doxpos = dxpos + dxsize - 1
      doypos = dypos + dysize - 1
c
c     Form an 'obscured extent' from the union of the overlapping
c     areas of all windows above the one to be cleared that cross
c     the extent to clear
c
      status = .FALSE.
c
      obsl = doxpos
      obst = doypos
      obsr = dxpos
      obsb = dypos
      noobs = 0
      do 1 i = MINWIN, MAXWIN
c
c        Window in use?
c
         if ( winnam(i) .ge. 0 ) then
c
c           Window on correct device?
c
            if ( winode(i) .eq. device ) then
c
c              Window not hidden?
c
               if ( winiss(i) ) then
c
c                 Above the one we are clearing?
c
                  if ( winstk(i) .gt. winstk(wid) ) then
c
c                    Intersects the area we are clearing?
c
                     if ( uixint (dxpos,dypos,dxsize,dysize,
     +                 winxpo(i),winypo(i),winxsi(i),winysi(i)) ) then
c
c                       Yes.  Accumulate into obscuring extent
c
                        if ( winxpo(i) .le. obsl ) then
                           if ( winxpo(i) .gt. dxpos ) then
                              obsl = winxpo(i)
                           else
                              obsl = dxpos
                           endif
                        endif
                        if ( winypo(i) .le. obst ) then
                           if ( winypo(i) .gt. dypos ) then
                              obst = winypo(i)
                           else
                              obst = dypos
                           endif
                        endif
                        itemp = winxpo(i) + winxsi(i) - 1
                        if ( itemp .ge. obsr ) then
                           if ( itemp .lt. doxpos ) then
                              obsr = itemp
                           else
                              obsr = doxpos
                           endif
                        endif
                        itemp = winypo(i) + winysi(i) - 1
                        if ( itemp .ge. obsb ) then
                           if ( itemp .le. doypos ) then
                              obsb = itemp
                           else
                              obsb = doypos
                           endif
                        endif
c
c                       Increment number of obscuring rectangles
c
                        noobs = noobs + 1
                     endif
                  endif
               endif
            endif
         endif
1     continue
c
c     Was the bit we are to clear actually obscured?
c
      if ( noobs .eq. 0 ) then
c
c        No, so we can just clear the bit we were asked to
c
         lclear = dxpos
         tclear = dypos
         sizex = dxsize
         sizey = dysize
         status = devclr (device, colour, lclear, tclear, sizex, sizey)
      else
c
c        Yes. Now have obscuring extent in device coordiates.  Clear
c        from the edges of the area to clear to the edges of the
c        obscuring extent.
c
c        First down to top...
c
         lclear = dxpos
         tclear = dypos
         sizex = dxsize
         sizey = obst - dypos
         status = devclr (device, colour, lclear, tclear, sizex, sizey)
c
c        Next across to left...
c
         lclear = dxpos
         tclear = obst
         sizex = obsl - dxpos
         sizey = obsb - obst + 1
         status = devclr (device, colour, lclear, tclear, sizex, sizey)
c
c        Next across from right...
c
         lclear = obsr + 1
         tclear = obst
         sizex = doxpos - obsr
         sizey = obsb - obst + 1
         status = devclr (device, colour, lclear, tclear, sizex, sizey)
c
c        Lastly down from bottom...
c
         lclear = dxpos
         tclear = obsb + 1
         sizex = dxsize
         sizey = doypos - obsb
         status = devclr (device, colour, lclear, tclear, sizex, sizey)
c
c        If only one obscuring extent, all done
c
         if ( noobs .gt. 1 ) then
c
c           Ok, now life gets complicated!  We need to split area to be
c           cleared into a set of unobscured horizontal strips.
c
            tclear = obst
            bclear = tclear
2           continue
            if ( bclear .lt. obsb ) then
c
c              Find vertical extent of next complete strip to clear.
c
               higbot = obsb + 1
               higtop = obsb + 1
               lclear = obsl
               rclear = obsr
               do 6 i = winstk(wid), MAXWIN
                  if ( i .eq. winstk(wid) ) goto 6
                  do 4 j = MINWIN, MAXWIN
                     if ( (winnam(j) .ge. 0) .and.
     +                    (winode(j) .eq. device) .and.
     +                    (winstk(j) .eq. i ) .and.
     +                     winiss(j) ) goto 5
4                 continue
c
c                 Empty stack position.  Try again
c
                  goto 6
5                 continue
c
c                 Window j is now the one at stack position i.  See if
c                 it changes the highest top or bottom of the strip
c
                  wintop = winypo(j)
                  winlef = winxpo(j)
                  winbot = winypo(j) + winysi(j) - 1
                  winrig = winxpo(j) + winxsi(j) - 1
                  if (winlef .le. rclear .and. winrig .ge. lclear) then
                     if (wintop .gt. tclear .and. wintop .le. obsb) then
                        if ( wintop .lt. higtop ) then
                           higtop = wintop
                        endif
                     endif
                     if (winbot .ge. tclear .and. winbot .le. obsb) then
                        if ( winbot .lt. higbot ) then
                           higbot = winbot
                        endif
                     endif
                  endif
6              continue
c
c              Adjust the bottom of the area to be cleared
c
               if ( higtop .lt. higbot ) then
                  bclear = higtop - 1
               else
                  if ( higtop .eq. higbot ) then
                     if ( higbot .eq. obsb ) then
                        bclear = higbot
                     else
                        bclear = higtop - 1
                     endif
                  else
                     bclear = higbot
                  endif
               endif
               if ( bclear .gt. obsb ) bclear = obsb
c
c              Can now begin clearing the strip.  Need now to split it
c              into segments along the X direction
c
7              continue
               if ( .not. wdxfns ( device, wid, obsl, obsr, tclear,
     +                             lclear, bclear, rclear, ledge,
     +                             redge ) ) goto 8
                  rclear = ledge - 1
c
c                 Clear segment
c
                  sizex = rclear - lclear  + 1
                  sizey = bclear - tclear + 1
                  status = devclr ( device, colour, lclear, tclear,
     +                              sizex, sizey )
                  lclear = redge + 1
                  rclear = obsr
               goto 7
8              continue
c
c              Clear last segment
c
               sizex = rclear - lclear  + 1
               sizey = bclear - tclear + 1
               status = devclr ( device, colour, lclear, tclear,
     +                           sizex, sizey )
               tclear = bclear + 1
               goto 2
            else
c
c              All done!!!
c
               goto 9
            endif
9           continue
         endif
      endif
c
c     All done
c
      wdxcow = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION WDXFNS ( DEVICE, WID, OBSL, OBSR, TCLEAR,
c                           LCLEAR, BCLEAR, RCLEAR, LEDGE, REDGE )
c       ----------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides
c
c       integer wid : INPUT - the identifier of the window to clear.
c
c       integer obsl, obsr : INPUT - the left and right sides of the
c                            obscured extent of area to clear.
c
c       integer tclear, lclear : INPUT - the top left of the area to
c                                clear, in device coordinates.
c
c       integer bclear, rclear : INPUT - the bottom right of the area to
c                                clear, in device coordinates.
c
c       integer ledge, redge : OUTPUT - the left and right edges of the
c                              leftmost contiguous obscured segment
c                              which spans the given area to clear.
c
c       Finds the leftmost contiguous obscured segment which spans the
c       given area to clear.
c
c       Function returns TRUE if spanning segment found, otherwise
c       FALSE.  LEDGE and REDGE are always returned correctly for use.
c
c----------------------------------------------------------------------
c
      logical function wdxfns ( device, wid, obsl, obsr, tclear, lclear,
     +                          bclear, rclear, ledge, redge )
c     ==================================================================
c
      integer device, wid, obsl, obsr, tclear, lclear, bclear
      integer rclear, ledge, redge
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Return value indicating segment found
      logical found
c
c     True if right edge already found
      logical rfound
c
c     Left, top, right and bottom edges of window.  Left & right edges
c     adjusted to edges of area to clear
      integer winlef, wintop, winrig, winbot
c
c     Identifier of rightmost window
      integer rigwin
c
c     Loop counters
      integer i,j
c
      found  = .FALSE.
      rfound = .FALSE.
      ledge = rclear
      redge = rclear
c------------
c      if ( lclear .lt. rclear ) then
c------------
      if ( lclear .le. rclear ) then
c
c        Walk up the stack to all windows above one being cleared seeing
c        if they cross the area to be cleared
c
         do 1 i = winstk(wid), MAXWIN
            if ( i .eq. winstk(wid) ) goto 1
            do 2 j = MINWIN, MAXWIN
               if ( (winnam(j) .ge. 0) .and.
     +              (winode(j) .eq. device) .and.
     +              (winstk(j) .eq. i ) .and.
     +               winiss(j) ) goto 3
2           continue
c
c           Empty stack position.  Try again
c
            goto 1
3           continue
c
c           Window j is now the one at stack position i.
c           Work out the window edges.
c
            winlef = winxpo(j)
            if ( winlef .lt. lclear ) winlef = obsl
            wintop = winypo(j)
            winrig = winxpo(j) + winxsi(j) - 1
            if ( winrig .gt. rclear ) winrig = obsr
            winbot = winypo(j) + winysi(j) - 1
c
c           See if it spans the area to be cleared.
c
            if ( wintop .le. bclear .and. winbot .ge. tclear ) then
c
c              Yes, so see if this contributes to either end of
c              the currently calculated obscuring segment.
c
               if ( winlef .ge. lclear .and. winlef .le. ledge ) then
                  ledge = winlef
c
c                 If right edge found, see if we need to swap it if
c                 this window is lefter than the one which formed
c                 right edge
c
                  if ( rfound ) then
                     if ( winxpo(j) .lt. winxpo(rigwin) ) then
                        if ( winxpo(j) + winxsi(j) - 1 .lt.
     +                       winxpo(rigwin) ) then
                           redge = winrig
                           rigwin = j
                           rfound = .TRUE.
                        endif
                     endif
                  endif
                  found = .TRUE.
               endif
               if ( winrig .ge. ledge .and. winlef .le. redge + 1 ) then
                  if ( rfound ) then
                     if ( winrig .gt. redge ) then
                        rigwin = j
                        redge = winrig
                     endif
                  else
                     rigwin = j
                     redge = winrig
                  endif
                  found  = .TRUE.
                  rfound = .TRUE.
               endif
            endif
1        continue
      endif
c
c     All done
c
      wdxfns = found
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
