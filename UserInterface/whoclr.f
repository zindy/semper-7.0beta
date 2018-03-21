c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WHOCLR ( DEVICE,COLOUR,WID,XPOS,YPOS,
c                                 XSIZE,YSIZE )
c       -------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window to be
c                        cleared is located
c
c       integer wid : INPUT - the window to be cleared.
c
c       integer colour : INPUT - the colour the area is to be
c                        cleared to.
c
c       integer xpos, ypos : INPUT - the position of the top left of
c                            the window in device coordinates.
c
c       integer xsize, ysize : INPUT - the size of the window in device
c                              coordinates.
c
c	Clears an area of a window on the framestore.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function whoclr (device,wid,colour,xpos,ypos,xsize,ysize)
c     =================================================================
c
      integer device
      integer wid
      integer colour
      integer xpos, ypos, xsize, ysize
c
c     CALLED FUNCTIONS:
c
c     Clears a window on a dumb screen
      logical wdxclr
c
      whoclr = wdxclr ( device,wid,colour,xpos,ypos,xsize,ysize )
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
