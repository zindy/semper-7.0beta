c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXHID ( DEVICE, WID )
c       --------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window
c                                is located.
c
c       integer wid : INPUT - the identifier of the window to hide.
c
c	Hides a window off the screen.  Adjusts the panel stack to take
c       account of the hide, clears the area of the screen occupied by
c       the window, and repaints all windows over the appropriate extent
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxhid ( device, wid )
c     ======================================
c
      integer device
      integer wid
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
c     Loop counter
      integer i
c
c     Physical index of window name store
      integer ind
c
c     Length of window name
      integer length
c
c     Stack position of window we are dealing with
      integer stkpos
c
c     Highest panel we need to redraw
      integer maxstk
c
c     Currently set clipping rectangle
      integer xcl, ycl, xcls, ycls
c
c     CALLED FUNCTIONS:
c
c     Clears an area of a window
      logical wdxclr
c
c     Draws a box
      logical wdxbox
c
c     Repaints an area of a device
      logical uixdre
c
c     Gets physical index of store
      logical pindex
c
c     Finds string length
      integer ustlen
c
c     Save and set the clipping rectangle to the area we are hiding
c
      call uixgcl ( xcl, ycl, xcls, ycls )
      call uixscl ( winxpo(wid), winypo(wid), winxsi(wid), winysi(wid) )
c
c     Clear the entire area of the panel to screen background
c
      status = wdxclr ( device, wid, 0, 0, 0, winxsi(wid), winysi(wid) )
c
c     Adjust the panel stack.  Move all windows below the one we are
c     hiding up the stack by one.
c
      do 1 i = MINWIN, MAXWIN
c
c        Window in use?
c
         if ( winnam(i) .ge. 0 ) then
c
c           On the correct device?
c
            if ( winode(i) .eq. device ) then
c
c              Window not hidden?
c
               if ( winstk(i) .ne. 0 ) then
c
c                 Above the one we are hiding?
c
                  if ( winstk(i) .lt. winstk(wid) ) then
c
c                    Adjust it!
c
                     winstk(i) = winstk(i) + 1
                  endif
               endif
            endif
         endif
1     continue
c
c     And set this panel as hidden
c
      maxstk = winstk(wid)
      winstk(wid) = 0
c
c     Update the obscured flags for the window stack
c
      call uixobs ( device )
c
c     Now repaint the window edges on the lower panels
c
      do 2 stkpos = MINWIN, maxstk
         do 3 i = MINWIN, MAXWIN
            if ( winnam(i) .ge. 0 ) then
               if ( winode(i) .eq. device ) then
                  if ( winstk(i) .eq. stkpos ) then
                     if ( winnam(i) .gt. 0 ) then
                        status = pindex ( winnam(i), ind )
                        length = ustlen ( cstore(ind:) )
                     else
                        ind = 1
                        length = 0
                     endif
c
c                    Clear the damaged area back to window background
c
                     status = wdxclr ( device, winwid(i), winbgc(i),
     +                                 0, 0, winxsi(i), winysi(i) )
                     status = wdxbox ( device, winwid(i), winbwi(i),
     +                                 winfgc(i), winbgc(i),
     +                                 cstore(ind:), length, INVERS,
     +                                 winhor(i), 0,0, winxsi(i),
     +                                 winysi(i) )
                  endif
               endif
            endif
3        continue
2     continue
c
c     And repaint the device over the extent previously occupied by the
c     window
c
      status = uixdre ( device, maxstk, winxpo(wid), winypo(wid),
     +                  winxsi(wid), winysi(wid) )
c
      if ( status ) then
c
c        Error - couldn't hide window
c
         uiferr = HIDWIN
      endif
c
c     Restore the clipping rectangle
c
      call uixscl ( xcl, ycl, xcls, ycls )
c
      wdxhid = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
