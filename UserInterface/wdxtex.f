c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXTEX ( DEVICE,WID,FGC,BGC,STYLE,XPOS,YPOS,
c                                 STRING, LENGTH )
c       ---------------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window into
c                        which text is to be written is located.
c
c       integer wid : INPUT - the window into which text is to be
c                     written.
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours in which to write the text.
c
c       integer style : INPUT - the style (NORMAL, INVERS, UNDERL) in
c                       which to write the text.
c
c       integer xpos, ypos : INPUT - the top left position for the text
c
c       character*(*) string : INPUT - the string to be written.
c
c       integer length : INPUT - the length of the string.
c
c	Writes a string to the requested window, in the appropriate
c       colours and style, on a dumb device.  Performs clipping, then
c       writes text out.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxtex ( device, wid, fgc, bgc, style, xpos,
     +                          ypos, string, length )
c     ==========================================================
c
      integer device, wid, fgc, bgc, style
      integer xpos, ypos, length
      character*(*) string
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
c     End position of unclipped string
      integer endpos
c
      integer start
      integer len
      integer ledge, redge
c
c     Position at which to write text, device coords.
      integer x, y
c
c     CALLED FUNCTIONS:
c
c     Writes text on a device
      logical devtex
c
c     Finds unobscured segment of area of window
      logical wdxfst
c
c     Check the string to be written against the clipping rectangle
c
      y = ypos + winypo(wid)
      if ( y .ge. clipt .and. y .le. clipb ) then
         start = 1
         x = xpos + winxpo(wid)
         endpos = x + length - 1
         if ( x .le. clipr .and. endpos .ge. clipl ) then
            if ( x .lt. clipl ) then
               start = clipl - x + 1
               x = clipl
            endif
            if ( endpos .gt. clipr ) then
               endpos = clipr
            endif
         else
c
c           String not in clipping rectangle
c
            status = .FALSE.
            goto 3
         endif
      else
c
c        String not in clipping rectangle
c
         status = .FALSE.
         goto 3
      endif
c
c     If window not obscured, can do quick write
c
      if ( .not. winobs(wid) ) then
         len = endpos - x + 1
         status = devtex ( device, x, y, string(start:), len,
     +                     fgc, bgc, style )
      else
c
c        Window is obscured.  Write out any unobscured bits
c        of the string
c
1        continue
c
c           Find next unobscured chunk
c
            if ( .not. wdxfst ( device, wid, x, y, endpos,
     +                          ledge, redge ) ) goto 2
c
c              Write text out
c
               len = ledge - x
               status = devtex ( device, x, y, string(start:),
     +                           len, fgc, bgc, style )
               start = start + redge - x + 1
               x = redge + 1
            goto 1
2        continue
c
c        Write out last chunk
c
         len = ledge - x
         if ( len .gt. 0 ) then
            status = devtex ( device, x, y, string(start:),
     +                        len, fgc, bgc, style )
         endif
      endif
c
c     All done
c
3     continue
c
      wdxtex = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function wdxfst ( device, wid, x, y, endx, ledge, redge )
c     =================================================================
c
      integer device, wid, x, y, endx, ledge, redge
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
c     Window which generated right end of area
      integer rigwin
c
c     End position of unclipped string
      integer endpos
c
c     Left, top, right and bottom edges of window.  Left & right edges
c     adjusted to edges of area to clear
      integer winlef, wintop, winrig, winbot
c
c     Loop counters
      integer i, j
c
      found  = .FALSE.
      rfound = .FALSE.
      endpos = endx
      ledge = endpos + 1
      redge = endpos + 1
c
c     Walk up the stack to all windows above one being written into
c     seeing if they cross the area to be written into.  Note we have
c     to scan left to right across the area we are looking at
c
      do 1 i = winstk(wid), MAXWIN
         if ( i .eq. winstk(wid) ) goto 1
         do 2 j = MINWIN, MAXWIN
            if ( (winnam(j) .ge. 0) .and.
     +           (winode(j) .eq. device) .and.
     +           (winstk(j) .eq. i ) .and.
     +            winiss(j) ) goto 3
2        continue
c
c        Empty stack position.  Try again
c
         goto 1
3        continue
c
c        Window j is now the one at stack position i.  See if it
c        crosses the string position
c
         wintop = winypo(j)
         winbot = winypo(j) + winysi(j) - 1
         if ( y .ge. wintop .and. y .le. winbot ) then
            winlef = winxpo(j)
            if ( winlef .lt. x ) winlef = x
            winrig = winxpo(j) + winxsi(j) - 1
            if ( winrig .gt. endpos ) winrig = endpos
            if ( x .le. winrig .and. endpos .ge. winlef ) then
c
c              Yes, so see if this contributes to either end of
c              the currently calculated obscuring segment.
c
               if ( winlef .ge. x .and. winlef .le. ledge ) then
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
         endif
1     continue
c
c     All done
c
      wdxfst = found
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
