c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WDXSHO ( DEVICE, WID )
c       --------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the device resides
c
c       integer wid : INPUT - the identifier of the window to show.
c
c	Shows a window on screen.  Checks the window will fit the screen,
c       and moves it if necessary.  Clears the approriate area of the
c       screen to the window background colour, and draws the window
c       border and window name.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wdxsho ( device, wid )
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
c     Physical index of string
      integer ind
c
c     Length of string
      integer length
c
c     Position and size of the window
      integer xpos, ypos, xsize, ysize
c
c     Maximum coordinates of the device
      integer dxmax, dymax
c
c     Currently set clipping rectangle
      integer xcl, ycl, xcls, ycls
c
c     CALLED FUNCTIONS:
c
c     Clears and area of a device
      logical devclr
c
c     Gets physical index of a string
      logical pindex
c
c     Finds string length
      integer ustlen
c
c     Draws a box
      logical wdxbox
c
c     Draws the contents of a panel
      logical uixpdr
c
c     Draws the contents of a scrolling area
      logical uixsad
c
c     Check window will fit
c
      status = .TRUE.
      xpos  = winxpo(wid)
      ypos  = winypo(wid)
      if ( xpos .lt. 0 ) xpos = 0
      if ( ypos .lt. 0 ) ypos = 0
      xsize = winxsi(wid)
      ysize = winysi(wid)
      dxmax = devxsi(device) - 1
      dymax = devysi(device) - 1
      if ( xpos + xsize - 1 .gt. dxmax ) then
         xpos = dxmax - xsize + 1
      endif
      if ( ypos + ysize - 1 .gt. dymax ) then
         ypos = dymax - ysize + 1
      endif
c
c     Now check position is still on the device
c
      if ( xpos .ge. 0 .and. ypos .ge. 0 ) then
c
c        Save position
c
         status = .FALSE.
         winxpo(wid) = xpos
         winypo(wid) = ypos
c
c        Save and set clipping rectangle
c
         call uixgcl ( xcl, ycl, xcls, ycls )
         call uixscl ( xpos, ypos, xsize, ysize )
c
c        Clear the area to background
c
         status = devclr ( device,winbgc(wid),xpos,ypos,xsize,ysize )
         if ( .not. status ) then
c
c           Adjust the window stack postions.  Move all windows in the
c           stack above the one we are showing down by one.
c
            do 1 i = MINWIN, MAXWIN
c
c              Window in use?
c
               if ( winnam(i) .ge. 0 ) then
c
c                 Window on correct device?
c
                  if ( winode(i) .eq. device ) then
c
c                     Window not hidden?
c
                      if ( winiss(i) ) then
c
c                        Above the one we are showing?
c
                         if ( winstk(i) .gt. winstk(wid) ) then
c
c                           Adjust it!!
c
                            winstk(i) = winstk(i) - 1
                         endif
                      endif
                  endif
               endif
1           continue
c
c           And set the one we are showing to the top
c
            winstk(wid) = MAXWIN
c
c           Now update window obscured info.
c
            call uixobs ( device )
c
c           This window now top, so definitely not obscured
c
            winobs(wid) = .FALSE.
c
c           Now paint the window border and the window name
c
            if ( winnam(wid) .gt. 0 ) then
               status = pindex ( winnam(wid), ind )
               length = ustlen ( cstore(ind:) )
            else
               ind    = 1
               length = 0
            endif
            status = wdxbox ( device, wid, winbwi(wid), winfgc(wid),
     +                        winbgc(wid), cstore(ind:), length,
     +                        INVERS, panhor(wid), 0, 0, xsize, ysize )
c
c           Now paint the window contents, ONLY IF THE WINDOW IS SHOWING
c           This is a nasty fix so that windows do not get drawn twice
c           when they are first shown, so callers must make sure
c           winiss(wid) is set correctly before calling this
c
            if ( .not. status ) then
               if ( winiss(wid) ) then
c
c                 Draw the correct thing
c
                  if ( wid .ge. MINPAN .and. wid .le. MAXPAN ) then
                     status = uixpdr ( wid, 1, 1, xsize - 2, ysize - 2 )
                  else if ( wid .ge. MINSCR .and. wid .le. MAXSCR ) then
                     status = uixsad ( wid, 1, 1, xsize - 2, ysize - 2 )
                  endif
               endif
            endif
         endif
c
c        Reset the clipping rectangle
c
         call uixscl ( xcl, ycl, xcls, ycls )
c
c        Check for error
c
         if ( status ) then
            uiferr = SHOWIN
         endif
      else
c
c        Error - window will not fit device
c
         uiferr = NOTFIT
         status = .TRUE.
      endif
c
      wdxsho = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
