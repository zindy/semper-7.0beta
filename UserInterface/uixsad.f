c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXSAD ( SID, XPOS, YPOS, XSIZE, YSIZE )
c       ---------------------------------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area
c                             to draw
c
c       integer xpos, ypos : INPUT - the top left corner of the area
c                                    within the scrolling area within
c                                    which to draw.
c
c       integer xsize, ysize : INPUT - the size of the area within
c                              which to draw
c
c       Draws a specified area of a scrolling area.  Loops over line
c       buffers elements on the scrolling area drawing them.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixsad ( sid, xpos, ypos, xsize, ysize )
c     =========================================================
c
      integer sid
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
c      Save and set the clipping rectangle
c
      call uixgcl ( xcl, ycl, xcls, ycls )
      call uixscl ( xpos + scrxpo(sid), ypos + scrypo(sid),
     +              xsize, ysize )
c
c     Loop over all line buffers
c
      status = .FALSE.
      do 1 i = 1, scrnli(sid)
1     continue
c
c     Restore clipping rectangle
c
      call uixscl ( xcl, ycl, xcls, ycls )
      uixsad = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
