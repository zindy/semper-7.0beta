c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXCMP ( EID, MID, PID )
c       -----------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the id of the cell on the menu
c
c       integer mid : INPUT - the id of the menu the cell is on
c
c       integer pid : INPUT - the id of the panel the cell is on
c
c       Takes care of any menu changes caused by cells being changed,
c       created, destroyed etc.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixcmp ( eid, mid, pid )
c     =========================================
c
      integer eid
      integer mid
      integer pid
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Window of owning panel
      integer wid
c
c     Position and size of the element
      integer xpos, ypos, xsize, ysize
c
c     Device owning panel is located on
      integer device
c
c     Panel background colour
      integer bgc
c
c     CALLED FUNCTIONS:
c
c     Recalculates size of a menu
      logical uixmsi
c
c     Clears an area of a window
      logical winclr
c
c     Repaints a menu
      logical uifmdr
c
c     Hides a panel
      logical uifphi
c
c     Shows a panel
      logical uifpsh
c
      device = panode(pid)
      wid    = panwid(pid)
      bgc    = panbgc(pid)
c
      status = .FALSE.
      if ( paniss(pid) ) then
         if ( mid .gt. 0 ) then
c
c           Repaint menu
c
            xpos  = elexpo(mid)
            ypos  = eleypo(mid)
            xsize = elexsi(mid)
            ysize = eleysi(mid)
            status = winclr ( device, wid, bgc, xpos, ypos,
     +                        xsize, ysize )
            if ( .not. status ) then
c
c              ... Re-size the menu...
c
               status = uixmsi ( mid )
               if ( .not. status ) then
c
c                 ... and repaint the menu
c
                  status = uifmdr ( mid )
               endif
            endif
         else if ( mid .lt. 0 ) then
c
c           On a pulldown/popup menu: hide, resize and show the
c           menu again (if it is showing)
c
            status = uifphi ( pid )
            if ( .not. status ) then
               status = uixmsi ( -mid )
               if ( .not. status ) then
                  panisc(pid) = .FALSE.
                  call uixpsp ( pid, panxpo(pid), panypo(pid),
     +                          panxsi(pid), panysi(pid) )
                  status = uifpsh ( pid )
               endif
            endif
         endif
      else
         if ( mid .ne. 0 ) then
c
c           If popup/pulldown, mark panel as not created
c           and recalcutate size
c
            if ( mid .lt. 0 ) then
               status = uixmsi ( -mid )
               panisc(pid) = .FALSE.
               call uixpsp ( pid, panxpo(pid), panypo(pid),
     +                       panxsi(pid), panysi(pid) )
            else
               status = uixmsi ( mid )
            endif
         endif
      endif
c
      uixcmp = status
c
      return
      IJUNK = EID
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
