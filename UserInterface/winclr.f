c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINCLR ( DEVICE,WID,COLOUR,XPOS,YPOS,
c                                 XSIZE,YSIZE )
c       ---------------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window to be
c                        cleared is located
c
c       integer wid : INPUT - the window to be cleared.
c
c       integer xpos, ypos : INPUT - the position of the top left of the
c                            area to be cleared in device coordinates.
c
c       integer xsize, ysize : INPUT - the size of the area to be
c                              cleared in device coordinates.
c
c	Cleares an area of a window to a gine colour. Calls the
c       appropriate function to create a window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function winclr ( device, wid, colour, xpos, ypos,
     +                          xsize, ysize )
c     ==========================================================
c
      integer device, wid, colour
      integer xpos, ypos, xsize, ysize
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
c     CALLED FUNCTIONS:
c
c     Clears a window on host device
      logical whoclr
c
c     Clears a window on framestore device
      logical wfsclr
c
      if ( device .eq. HOST ) then
         status = whoclr ( device, wid, colour,
     +                     xpos, ypos, xsize, ysize )
      else
         status = wfsclr ( device, wid, colour,
     +                     xpos, ypos, xsize, ysize )
      endif
c
      winclr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
