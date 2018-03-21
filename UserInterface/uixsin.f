c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXSIN ( SID, DEVICE )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area to
c                     initialise.
c
c       integer device : INPUT - the device the scrolling area is
c       created on.
c
c       Sets up default values for scrolling area sid.
c
c----------------------------------------------------------------------
c
      subroutine uixsin ( sid, device )
c     =================================
c
      integer sid
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Initialise window data.
c
      call uixwin ( sid, device )
c
c     Initialise scrolling area data.
c
      scrlin(sid) = 0
      scrnli(sid) = 0
      scrnch(sid) = 0
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
