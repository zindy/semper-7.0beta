c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXSOF ( DEVICE, XOFF, YOFF, XSIZE, YSIZE )
c       ------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device whose offset is to be
c                        changed.
c
c       integer xpos, ypos : INPUT - the new device offset.
c
c       integer xsize, ysize : INPUT - the new device size.
c
c       Changes the device offset and size for the given device.
c
c----------------------------------------------------------------------
c
      subroutine uixsof ( device, xoff, yoff, xsize, ysize )
c     ======================================================
c
      integer device, xoff, yoff, xsize, ysize
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Set the offset for the requested device
c
      if ( device .eq. HOST ) then
         devxof(HOST) = xoff
         devyof(HOST) = yoff
         devxsi(HOST) = xsize
         devysi(HOST) = ysize
      else if ( device .eq. FSTORE ) then
         devxof(FSTORE) = xoff
         devyof(FSTORE) = yoff
         devxsi(FSTORE) = xsize
         devysi(FSTORE) = ysize
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
