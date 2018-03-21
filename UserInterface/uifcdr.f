c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCDR ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell to draw
c
c       Draws the given cell, both contents and highlighting.  The
c       extent of the cell is cleared to the cell background colour.
c       If the cell type is TEXT, then the text as stored is drawn as
c       the contents.  If the cell is ICONIC, then the icon is loaded
c       from disc, and drawn.  The name of the cell is drawn, if defined
c       The check or tick marker, in its current state, is drawn if
c       needed. An error is generated if the panel owning the cell is
c       not showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcdr ( eid )
c     ==============================
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
c     Return status
      logical status
c
c     Position to write text etc.
      integer xpos, ypos
c
c     Size of cell
      integer xsize, ysize
c
c     Identifier of owning panel
      integer pid
c
c     Identifier of window of owning panel
      integer wid
c
c     Device window is located on
      integer device
c
c     Foreground and background colours of text
      integer fgc,bgc
c
c     Physical index of contents text
      integer ind
c
c     Length of contents text
      integer length
c
c     Style to draw text contents in
      integer style
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of string
      logical pindex
c
c     Finds string length
      integer USTLEN
c
c     Clears an area of a window
      logical winclr
c
c     Writes text in a window
      logical wintex
c
c     Draws a box, with banner, in a window
      logical winbox
c
c     Draws cell highlighting marker
      logical uixcdh
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check panel is showing
c
         if ( paniss(celopa(eid)) ) then
c
c           Check element id is valid
c
            if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
               if ( celnam(eid) .ge. 0 ) then
c
c                 Clear the area the cell is about to occupy
c
                  xpos = elexpo(eid)
                  ypos = eleypo(eid)
                  xsize = celxsi(eid)
                  ysize = celysi(eid)
                  pid = celopa(eid)
                  wid = panwid(pid)
                  device = winode(wid)
                  fgc = celfgc(eid)
                  bgc = celbgc(eid)
                  status = winclr ( device, wid, bgc, xpos, ypos,
     +                              xsize, ysize )
                  if ( .not. status ) then
c
c                    If cell is named, draw the box around the cell
c
                     if ( celnam(eid) .gt. 0 ) then
                        status = pindex ( celnam(eid), ind )
                        length = USTLEN ( cstore(ind:) )
                        status = winbox ( device, wid, 1, fgc, bgc,
     +                                    cstore(ind:), length,
     +                                    NORMAL, celhor(eid), xpos,
     +                                    ypos, xsize, ysize )
                        xpos = xpos + 1
                        ypos = ypos + 1
                     else if ( celbox(eid) ) then
                        status = winbox ( device, wid, 1, fgc, bgc,
     +                                    ' ', 0,
     +                                    NORMAL, celhor(eid), xpos,
     +                                    ypos, xsize, ysize )
                        xpos = xpos + 1
                        ypos = ypos + 1
                     endif
c
c                    Now, if the cell has contents, draw them
c
                     if (.not. status .and. celcon(eid) .gt. 0) then
c
c                       If needed, draw the highlight marker
c
                        if ( celsty(eid) .eq. CHECK .or.
     +                       celsty(eid) .eq. TICK ) then
                           status = uixcdh ( eid, celcyc(eid) )
                           xpos = xpos + 1
                        endif
c
c                       Now the contents, in the correct style
c                       for the highlighting.
c
                        if ( .not. status ) then
                           status = pindex ( celcon(eid), ind )
                           length = USTLEN ( cstore(ind:) )
                           if ( celsty(eid) .eq. INVERT ) then
                              style = celcyc(eid)
                           else
                              style = NORMAL
                           endif
                           status = wintex ( device, wid, fgc, bgc,
     +                                       style, xpos, ypos,
     +                                       cstore(ind:), length )
                        endif
                     endif
                  endif
               else
c
c                 Error - invalid cell id
c
                  uiferr = BADCEL
                  status = .TRUE.
               endif
            else
c
c              Error - invalid cell id
c
               uiferr = BADCEL
               status = .TRUE.
            endif
         else
c
c           Error - panel is not showing
c
            uiferr = NOTSHO
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
      uifcdr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
