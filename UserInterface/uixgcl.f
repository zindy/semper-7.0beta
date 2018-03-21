c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXGCL ( XPOS, YPOS, XSIZE, YSIZE )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer xpos, ypos : OUTPUT - the top left corner of the clip
c                            rectangle in device coordinates
c
c       integer xsize, ysize : OUTPUT - the size of the clip rectangle
c                              in device coordinates
c
c       Returns the current clip rectangle.
c
c----------------------------------------------------------------------
c
      subroutine uixgcl ( xpos, ypos, xsize, ysize )
c     ==============================================
c
      integer xpos, ypos
      integer xsize, ysize
c
      include 'UIFCOM'
      include 'UIXCOM'
c
      xpos = clipl
      xsize = clipr - clipl + 1
      ypos = clipt
      ysize = clipb - clipt + 1
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
