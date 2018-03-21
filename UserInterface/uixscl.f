c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXSCL ( XPOS, YPOS, XSIZE, YSIZE )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer xpos, ypos : INPUT - the top left corner of the clip
c                            rectangle in device coordinates
c
c       integer xsize, ysize : INPUT - the size of the clip rectangle in
c                              device coordinates
c
c       Sets the clip rectangle to that specified.
c
c----------------------------------------------------------------------
c
      subroutine uixscl ( xpos, ypos, xsize, ysize )
c     ==============================================
c
      integer xpos, ypos
      integer xsize, ysize
c
      include 'UIFCOM'
      include 'UIXCOM'
c
      clipl = xpos
      clipr = xpos + xsize - 1
      clipt = ypos
      clipb = ypos + ysize - 1
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
