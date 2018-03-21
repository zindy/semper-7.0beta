c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFDRF ( DEVICE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - The device to be refreshed (valid
c                        values: HOST, FSTORE)
c
c       Repaints all panels on the given device.  Clears the device,
c       then paints all panels from the bottom of the panel stack
c       upwards.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifdrf ( device )
c     ==================================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Copy of the window stack
      integer locwin(MXNWIN)
c
c     Return status
      logical status
c
c     Stack positions used as loop counter for repainting window stack
      integer stkpos
c
c     Loop counter
      integer i
c
c     Saved 'fixed panel' flag
      logical isfix
c
c     Saved 'panel is showing' flag
      logical isshow
c
c     CALLED FUNCTIONS:
c
c     Clears an area of a device
      logical devclr
c
c     Repaints an area of a device
c      logical uixdre
c
c     Shows a panel
      logical uifpsh
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        See if there are any windows on this device
c
         devhap(device) = .FALSE.
         do 10 i = MINWIN, MAXWIN
            locwin(i) = winstk(i)
            if ( winnam(i) .ge. 0 .and. winode(i) .eq. device ) then
               devhap(device) = .TRUE.
            endif
10       continue
c
c        Clear the device if there are panels on it
c
         status = .FALSE.
         if ( devhap(device) ) then
            status = devclr ( device, 0, 0, 0,
     +                        devxsi(device), devysi(device) )
            if ( .not. status ) then
c
c              Hide then show again all showing windows.  Walk up the
c              panel stack from the bottom
c

               do 1 stkpos = MINWIN, MAXWIN
c
c                 Find the panel at this stack position.  Must check
c                 panel is in use, on the correct device and not hidden
c                 as well as stack position
c
                  do 2 i = MINWIN, MAXWIN
                     if ( winnam(i) .ge. 0 .and. winode(i) .eq. device
     +                    .and. locwin(i) .eq. stkpos
     +                    .and. winiss(i) ) goto 3
2                 continue
c
c                 No showing window at this stack position.  Try next
c                 stack position: this one may not be in use.
c
                  goto 1
3                 continue
c
c                 Here when window(i) needs repainting.
c
                  if ( i .ge. MINPAN .and. i .le. MAXPAN ) then
c
c                     Panel.  Save its 'fixedness', and 'showingness',
c                     'unmove it' (taking account of it's positioning
c                     parameters) and show it again so it gets
c                     repainted.
c
                      isfix  = panfix(i)
                      isshow = paniss(i)
                      panfix(i) = .FALSE.
                      paniss(i) = .FALSE.
                      panxpo(i) = panxpo(i) +
     +                               (panxsi(i) / 2) * panhor(i)
                      panypo(i) = panypo(i) +
     +                               (panysi(i) / 2) * panver(i)
                      status = uifpsh ( i )
                      panfix(i) = isfix
                      paniss(i) = isshow
                      if ( status ) goto 4
                  else if ( i .ge. MINSCR .and. i .le. MAXSCR ) then
c
c                    Scrolling area.  Show it.
c
                  endif
1              continue
4              continue
            endif
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifdrf = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
