c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXPDR ( PID, XPOS, YPOS, XSIZE, YSIZE )
c       ---------------------------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel to draw
c
c       integer xpos, ypos : INPUT - the top left corner of the area
c                                    within the panel within which to
c                                    draw.
c
c       integer xsize, ysize : INPUT - the size of the area within
c                              which to draw
c
c       Draws a specified area of a panel.  Loops over all elements on
c       the panel.  If the element is in use, on the correct panel, and
c       intersects with the area to be drawn, it is drawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixpdr ( pid, xpos, ypos, xsize, ysize )
c     =========================================================
c
      integer pid
      integer xpos, ypos
      integer xsize, ysize
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
c     Loop counter
      integer i
c
c     Currently set clipping rectangle
      integer xcl, ycl, xcls, ycls
c
c     CALLED FUNCTIONS:
c
c     Checks if two rectangles intersect
      logical uixint
c
c     Draws an element
      logical uifedr
c
c      Save and set the clipping rectangle
c
      call uixgcl ( xcl, ycl, xcls, ycls )
      call uixscl ( xpos + panxpo(pid), ypos + panypo(pid),
     +              xsize, ysize )
c
c     Loop over all elements
c
      status = .FALSE.
      do 1 i = MINELE, MAXELE
c
c        In use?
c
         if ( elenam(i) .ge. 0 ) then
c
c           On this panel?
c
            if ( eleopa(i) .eq. pid ) then
c
c              Intersects area of interest?
c
               if ( uixint ( xpos, ypos, xsize, ysize, elexpo(i),
     +                       eleypo(i), elexsi(i), eleysi(i) ) ) then
                  status = uifedr ( i )
                  if ( status ) goto 2
               endif
            endif
         endif
1     continue
2     continue
c
c     Restore clipping rectangle
c
      call uixscl ( xcl, ycl, xcls, ycls )
      uixpdr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
