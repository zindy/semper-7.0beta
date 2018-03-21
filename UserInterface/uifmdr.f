c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFMDR ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the menu element to be
c                     drawn.
c
c       Draws given menu element.  If the menu background colour is
c       different from the background colour of its owning panel, then
c       extent of the menu is cleared to the menu background colour.
c       The menu name is drawn in the menu foreground colour, and all
c       the cells in turn are drawn.  The name will be drawn in a
c       special manner if the menu is one currently selected, to
c       indicate this fact.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifmdr ( eid )
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
c     Owning panel id
      integer pid
c
c     Id of owning panel's window
      integer wid
c
c     Device window resides on
      integer device
c
c     Forground and background colours of menu
      integer fgc, bgc
c
c     Position of menu
      integer xpos, ypos
c
c     Size of menu
      integer xsize, ysize
c
c     Physical index of store
      integer ind
c
c     Length of menu name string
      integer length
c
c     Style in which to draw menu name
      integer style
c
c     Loop counter
      integer i
c
c     CALLED FUNCTIONS:
c
c     Clears an area of a window
      logical winclr
c
c     Gets physical index of store
      logical pindex
c
c     Gets length of string
      integer USTLEN
c
c     Draws a box in a window
      logical winbox
c
c     Draws text in a window
      logical wintex
c
c     Draws a cell element
      logical uifcdr
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check menu id is valid
c
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN
     +        .and. mennam(eid) .ge. 0 ) then
c
            pid = menopa(eid)
            wid = winwid(pid)
            device = winode(wid)
            fgc = menfgc(eid)
            bgc = menbgc(eid)
            xpos = menxpo(eid)
            ypos = menypo(eid)
            xsize = menxsi(eid)
            ysize = menysi(eid)
c
c           Split on type of menu
c
            if ( mentyp(eid) .eq. FIXED ) then
c
c              Fixed menu.  Check owning panel is showing
c
               if ( paniss(pid) ) then
c
c                 Clear the area menu will cover to the menu
c                 background colour
c
                  status = winclr ( device, wid, bgc, xpos, ypos,
     +                              xsize, ysize )
                  if ( .not. status ) then
c
c                    Draw the enclosing box.  Find menu name first.
c
                     if ( mennam(eid) .gt. 0 ) then
                        status = pindex ( mennam(eid), ind )
                        length = USTLEN ( cstore(ind:) )
                     else
                        ind = 1
                        length = 0
                     endif
                     if ( .not. status ) then
c
c                       Set style of the menu name depending on
c                       whether it is active or not
c
                        if ( menisa(eid) ) then
                           style = INVERS
                        else
                           style = NORMAL
                        endif
c
c                       Do the box.
c
                        status = winbox ( device, wid, 1, fgc, bgc,
     +                                    cstore(ind:), length, style,
     +                                    menhor(eid), xpos, ypos,
     +                                    xsize, ysize )
c
c                       Now draw the cells of the thing
c
                        if ( .not. status ) then
                           do 1 i = MINCEL, MAXCEL
                              if ( celome(i) .eq. eid ) then
                                 status = uifcdr ( i )
                                 if ( status ) goto 2
                              endif
1                          continue
2                          continue
                       endif
                     endif
                  endif
               else
c
c                 Error - panel is not showing
c
                  uiferr = NOTSHO
                  status = .TRUE.
               endif
            else if ( mentyp(eid) .eq. PULLDN ) then
c
c              Pulldown menu.  Check owning panel is showing
c
               if ( paniss(pid) ) then
c
c                 Find menu name and length
c
                  if ( mennam(eid) .gt. 0 ) then
                     status = pindex ( mennam(eid), ind )
                     length = USTLEN ( cstore(ind:) )
                     if ( .not. status ) then
c
c                       Clear the area menu name will cover to the menu
c                       background colour
c
                        xsize = length
                        status = winclr ( device, wid, bgc, xpos, ypos,
     +                                    xsize, ysize )
                        if ( .not. status ) then
c
c                          Now draw menu name.
c
                           style = NORMAL
                           status = wintex ( device, wid, fgc, bgc,
     +                                       style, xpos, ypos,
     +                                       cstore(ind:), length )
                        endif
                     endif
                  else
c
c                    Since all we do for drawing pulldown menus is
c                    to draw their name, we have nothing to do if
c                    it has no name
c
                     status = .FALSE.
                  endif
               else
c
c                 Error - panel is not showing
c
                  uiferr = NOTSHO
                  status = .TRUE.
               endif
            else
c
c              Popup menu - nothing to do!
c
               status = .FALSE.
            endif
         else
c
c           Error - bad menu id
c
            uiferr = BADMEN
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
      uifmdr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
