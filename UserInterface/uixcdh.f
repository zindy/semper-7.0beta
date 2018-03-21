c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXCDH ( EID, STATE )
c       --------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose highlight
c                             is to be drawn.
c
c       integer state : INPUT - the state the cell is to drawn in.
c
c       Draws the highlighing marker for the given cell element.  Deals
c       only with CHECK and TICK highlighting cells.  Input values are
c       not checked!
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixcdh ( eid, state )
c     ======================================
c
      integer eid, state
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
c     Position of the highlighting
      integer xpos, ypos
c
c     Device owning panel is located on
      integer device
c
c     Window of owning panel
      integer wid
c
c     Cell foreground and background colours
      integer fgc, bgc
c
c     CALLED FUNCTIONS:
c
c     Clears an area of a window
      logical winclr
c
c     Draws a character in a window
      logical wintex
c
c     Calculate the highlighting position
c
      xpos = celxpo(eid)
      ypos = celypo(eid)
c
c     Adjust if cell named
c
      if ( celnam(eid) .gt. 0 .or. celbox(eid) ) then
         xpos = xpos + 1
         ypos = ypos + 1
      endif
c
c     Clear the area of the highlighting, and draw the new one
c
      device = panode(celopa(eid))
      wid    = panwid(celopa(eid))
      bgc    = celbgc(eid)
      fgc    = celfgc(eid)
      status = winclr ( device, wid, bgc, xpos, ypos, 1, 1 )
      if ( .not. status ) then
         if ( state .eq. TICKED ) then
            status = wintex ( device, wid, fgc, bgc, INVERS, xpos,
     +                        ypos, '+', 1 )
         else if ( state .eq. CROSS ) then
            status = wintex ( device, wid, fgc, bgc, INVERS, xpos,
     +                        ypos, '-', 1 )
         endif
      endif
c
      uixcdh = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
