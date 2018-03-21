c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTDR ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element to
c                     be drawn.
c
c       Draws given textfield element.  The extent of the textfield is
c       cleared to the textfield background colour.  The textfield name
c       and contents are drawn in the textfield foreground colour.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftdr ( eid )
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
c     Return status
      logical status
c
c     Position to write text etc.
      integer xpos, ypos
c
c     Size of textfield
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
c     Physical index of text
      integer ind
c
c     Length of text
      integer length
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
c     Draws the textfield contents
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
c           Check panel is showing
c
            if ( paniss(texopa(eid)) ) then
c
c              Clear the area the textfield is about to occupy
c
               xpos = texxpo(eid)
               ypos = texypo(eid)
               xsize = texxsi(eid)
               ysize = texysi(eid)
               pid = texopa(eid)
               wid = panwid(pid)
               device = winode(wid)
               fgc = texfgc(eid)
               bgc = texbgc(eid)
               status = winclr ( device, wid, bgc, xpos, ypos,
     +                              xsize, ysize )
               if ( .not. status ) then
c
c                 If textfield is named, draw the name
c
                  if ( texnam(eid) .gt. 0 ) then
                     status = pindex ( texnam(eid), ind )
                     length = USTLEN ( cstore(ind:) )
                     status = wintex ( device, wid, fgc, bgc, NORMAL,
     +                                 xpos, ypos, cstore(ind:),
     +                                 length )
                     if ( .not. status ) then
                        xpos = xpos + length
                        status = wintex ( device, wid, fgc, bgc, NORMAL,
     +                                    xpos, ypos, ': ', 2 )
                        xpos = xpos + 2
                     endif
                  endif
c
c                 Now, if the textfield has contents, draw them
c
                  if ( .not. status ) then
                     status = uixtdc ( eid, 1 )
                  endif
               else
c
c                 Error - invalid textfield id
c
                  uiferr = BADTEX
                  status = .TRUE.
               endif
            else
c
c              Error - invalid cell id
c
               uiferr = BADTEX
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
c     All done
c
      uiftdr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixtdc ( eid, start )
c     ======================================
c
      integer eid
      integer start
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
c     Physical index of text
      integer ind
c
c     Length of text
      integer length
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of string
      logical pindex
c
c     Finds string length
      integer USTLEN
c
c     Writes text in a window
      logical wintex
c
c     Draws the cursor
      logical uixxdc
c
c     Assume everything is OK, since we are called from sanitary
c     places.  If the textfield has contents, draw them.
c
      if ( texcon(eid) .gt. 0 ) then
         xpos = texxpo(eid)
         ypos = texypo(eid)
         pid = texopa(eid)
         wid = panwid(pid)
         device = winode(wid)
         fgc = texfgc(eid)
         bgc = texbgc(eid)
c
         if ( texnam(eid) .gt. 0 ) then
            status = pindex ( texnam(eid), ind )
            length = USTLEN ( cstore(ind:) )
            xpos = xpos + length + 2
         else
            status = .FALSE.
            ind    = 1
            length = 0
         endif
c
c        Now the contents, in the correct style
c
         if ( .not. status ) then
            status = pindex ( texcon(eid), ind )
            length = texlen(eid)
            length = length - start + 1
            xpos = xpos + start - 1
            ind  = ind + start - 1
            status = wintex ( device, wid, fgc, bgc, INVERS, xpos,
     +                        ypos, cstore(ind:), length )
c
c           Now do the cursor
c
            if ( .not. status ) then
               if ( texcpo(eid) .ge. start ) then
                  status = uixxdc ( eid )
               endif
            endif
         endif
      else
         status = .FALSE.
      endif
c
      uixtdc = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixxdc ( eid )
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
c     Return status
      logical status
c
c     Position to write text etc.
      integer xpos, ypos
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
c     Physical index of text
      integer ind
c
c     Length of text
      integer length
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of string
      logical pindex
c
c     Finds string length
      integer USTLEN
c
c     Writes text in a window
      logical wintex
c
c     Assume everything is OK, since we are called from sanitary
c     places.  Calcualte the what we need for drawing the cursor
c
      if ( texcon(eid) .gt. 0 ) then
         xpos = texxpo(eid)
         ypos = texypo(eid)
         pid = texopa(eid)
         wid = panwid(pid)
         device = winode(wid)
         fgc = texfgc(eid)
         bgc = texbgc(eid)
c
         if ( texnam(eid) .gt. 0 ) then
            status = pindex ( texnam(eid), ind )
            length = USTLEN ( cstore(ind:) )
            xpos = xpos + length + 2 + texcpo(eid) - 1
         else
            ind    = 1
            length = 0
            xpos = xpos + texcpo(eid) - 1
         endif
c
         status = pindex ( texcon(eid), ind )
         ind  = ind + texcpo(eid) - 1
c
c        Cursor is normal (i.e. visible) only if this textfield is
c        selected
c
         if ( eid .eq. seltex ) then
            status = wintex ( device, wid, fgc, bgc, NORMAL,
     +                        xpos, ypos, cstore(ind:), 1 )
         else
            status = wintex ( device, wid, fgc, bgc, INVERS,
     +                        xpos, ypos, cstore(ind:), 1 )
         endif
      else
         status = .FALSE.
      endif
c
      uixxdc = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixxmc ( eid, newpos )
c     =======================================
c
      integer eid, newpos
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
c     Cursor x position
      integer x
c
c     Physical index of character at cursor pos
      integer cind
c
c     Position to write text etc.
      integer xpos, ypos
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
c     Physical index of text
      integer ind
c
c     Length of text
      integer length
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of string
      logical pindex
c
c     Finds string length
      integer USTLEN
c
c     Writes text in a window
      logical wintex
c
c     Assume everything is OK, since we are called from sanitary
c     places.  Calcualte the what we need for drawing the cursor
c
      if ( texcon(eid) .gt. 0 ) then
         xpos = texxpo(eid)
         ypos = texypo(eid)
         pid = texopa(eid)
         wid = panwid(pid)
         device = winode(wid)
         fgc = texfgc(eid)
         bgc = texbgc(eid)
c
         if ( texnam(eid) .gt. 0 ) then
            status = pindex ( texnam(eid), ind )
            length = USTLEN ( cstore(ind:) )
            x = xpos + length + 2 + texcpo(eid) - 1
         else
            ind    = 1
            length = 0
            x = xpos + texcpo(eid) - 1
         endif
c
         status = pindex ( texcon(eid), ind )
c
c        Undraw the cursor where it is
c
         cind = ind + texcpo(eid) - 1
c
         status = wintex ( device, wid, fgc, bgc, INVERS,
     +                     x, ypos, cstore(cind:), 1 )
c
c        Now draw the cursor in its new position
c
         texcpo(eid) = newpos
         if ( texnam(eid) .gt. 0 ) then
            x = xpos + length + 2 + texcpo(eid) - 1
         else
            x = xpos + texcpo(eid) - 1
         endif
         cind = ind + texcpo(eid) - 1
c
c        Cursor is normal (i.e. visible) only if this textfield is
c        selected
c
         if ( eid .eq. seltex ) then
            status = wintex ( device, wid, fgc, bgc, NORMAL,
     +                        x, ypos, cstore(cind:), 1 )
         else
            status = wintex ( device, wid, fgc, bgc, INVERS,
     +                        x, ypos, cstore(cind:), 1 )
         endif
      else
         status = .FALSE.
      endif
c
      uixxmc = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
