c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DEVDSI ( DEVICE, XMIN, YMIN, XSIZE, YSIZE,
c                                 MXSCA, MYSCA, XOFF, YOFF, NCOLS )
c       -----------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which size etc. is
c                        required.
c
c       integer xmin, ymin : OUTPUT - The minimum coordinates available
c                            on the device.
c
c       integer xsize, ysize : OUTPUT - The size of the device.
c
c       integer mxsca, mysca : OUTPUT - The mouse to screen scale.
c
c       integer xoff, yoff : OUTPUT - Offset to be added to device
c                            coordinates for this device
c
c       integer ncols : OUTPUT - The number of colours the device
c                       supports
c
c       Returns the minimum coordiates, size, mouse scale and number of
c       colours of the requested display device.  The device is
c       initialised if necessary.
c
c       Function returns TRUE in case of error, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function devdsi ( device, xmin, ymin, xsize, ysize,
     +                          mxsca, mysca, xoff, yoff, ncols )
c     ===========================================================
c
      integer device, xmin, ymin, xsize, ysize, mxsca, mysca,
     +        xoff, yoff, ncols
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Gets size etc. of the host device
      logical dhodsi
c
c     Gets size etc. of the framestore device
      logical dfsdsi
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         devdsi = dhodsi ( xmin, ymin, xsize, ysize,
     +                     mxsca, mysca, xoff, yoff, ncols )
      else
         devdsi = dfsdsi ( xmin, ymin, xsize, ysize,
     +                     mxsca, mysca, xoff, yoff, ncols )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DEVDSH ( DEVICE )
c       ----------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device which is to be shut down.
c
c       Terminates use of the requested device.
c
c       Function returns TRUE in case of error, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function devdsh ( device )
c     ==================================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Terminates the host device
      logical dhodsh
c
c     Terminates the framstore device
      logical dfsdsh
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         devdsh = dhodsh ( 1 )
      else
         devdsh = dfsdsh ( 1 )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCSA ( DEVICE )
c       ----------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor state
c                        is to be saved.
c
c       Saves the state (position and style) of the cursor on the given
c       device.
c
c----------------------------------------------------------------------
c
      subroutine devcsa ( device )
c     ============================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocsa
      else
         call dfscsa
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCRE ( DEVICE )
c       ----------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor state
c                        is to be restored.
c
c       Restores the state (position and style) of the cursor on the
c       given device.
c
c----------------------------------------------------------------------
c
      subroutine devcre ( device )
c     ============================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocre
      else
         call dfscre
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCON ( DEVICE )
c       ----------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor is to
c                        be turned on (made visible)
c
c       Makes the cursor on the given device visible.
c
c----------------------------------------------------------------------
c
      subroutine devcon ( device )
c     ============================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocon
      else
         call dfscon
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCOF ( DEVICE )
c       ----------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor is to
c                        be turned off (made invisible)
c
c       Makes the cursor on the given device invisible.
c
c----------------------------------------------------------------------
c
      subroutine devcof ( device )
c     ============================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocof
      else
         call dfscof
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCMO ( DEVICE, XPOS, YPOS )
c       ----------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor is to
c                        be moved.
c
c       integer xpos, ypos : INPUT - the position (device coordinates)
c                            to which the cursor is to be moved.
c
c       Moves the cursor to the given position on the given device.
c
c----------------------------------------------------------------------
c
      subroutine devcmo ( device, xpos, ypos )
c     ========================================
c
      integer device, xpos, ypos
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocmo ( xpos, ypos )
      else
         call dfscmo ( xpos, ypos )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DEVCQU ( DEVICE, XPOS, YPOS )
c       ----------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device of which the cursor
c                        position is to be queried.
c
c       integer xpos, ypos : OUTPUT - the position (device coordinates)
c                            at which the cursor is currently
c
c       Returns the current position of the cursor on the given device.
c
c----------------------------------------------------------------------
c
      subroutine devcqu ( device, xpos, ypos )
c     ========================================
c
      integer device, xpos, ypos
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         call dhocqu ( xpos, ypos )
      else
         call dfscqu ( xpos, ypos )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DEVTEX ( DEVICE, XPOS, YPOS, STRING, LENGTH,
c                                 FGC, BGC, STYLE )
c       -------------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which the text is to
c                        be drawn.
c
c       integer xpos, ypos : INPUT - the position (device coordinates)
c                            at which the text is to be drawn.
c
c       character*(*) string : INPUT - the string to be written.
c
c       integer length : INPUT - the length of the string.
c
c       integer fgc, bgc : INPUT - the foreground and background colours
c                          in which to write the text.
c
c       integer style : INPUT - the style (NORMAL or INVERS) in
c                       which to write the text.
c
c       Writes a text string at the given position in the given colours
c       and style on the given device.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function devtex ( device, xpos, ypos, string, length,
     +                          fgc, bgc, style )
c     =============================================================
c
      integer device,xpos,ypos
      character*(*) string
      integer length, fgc, bgc, style
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Writes text on the host device
      logical dhotex
c
c     Writes text on the framestore device
      logical dfstex
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         devtex = dhotex ( xpos, ypos, string, length, fgc, bgc, style )
      else
         devtex = dfstex () ! xpos, ypos, string, length, fgc, bgc, style )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DEVCLR ( DEVICE, COLOUR, XPOS, YPOS,
c                                 XSIZE, YSIZE )
c       -----------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which an area is to
c                        be cleared.
c
c       integer colour : INPUT - the colour to which the area should
c                        be cleared.
c
c       integer xpos, ypos : INPUT - the position (device coordinates)
c                            of the top left of the area to be cleared.
c
c       integer xsize, ysize : INPUT - the size of the area to clear.
c
c       Clears an area of the given device to a given colour.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function devclr ( device, colour, xpos, ypos,
     +                          xsize, ysize )
c     =====================================================
c
      integer device, colour, xpos, ypos, xsize, ysize
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Clears an area on the host device
      logical dhoclr
c
c     Clears an area on the framestore device
      logical dfsclr
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         devclr = dhoclr ( colour, xpos, ypos, xsize, ysize )
      else
         devclr = dfsclr ( ) !! colour, xpos, ypos, xsize, ysize )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c     LOGICAL FUNCTION DEVHOR ( DEVICE, WIDTH, FGC, BGC, XSTART,
c                               XEND, YPOS, DLEFT, DRIGHT, DOWNWD )
c     -------------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which line is to be
c                        drawn.
c
c       integer width : INPUT - the width of the line.
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours in which to draw the line.
c
c       integer xstart, xend : INPUT - the start & end positions of the
c                              line (device coordinates)
c
c       integer ypos : INPUT - the y position of the line
c
c       logical dleft, dright : INPUT - flags indicating if left and
c                               right ends of line need verticals.
c
c       logical downwd : INPUT - flag indicating direction of verticals.
c
c       Draws a horizontal line on a device.  Special action is taken
c       at ends if needed if it is meeting a vertical line.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function devhor ( device, width, fgc, bgc, xstart,
     +                          xend, ypos, dleft, dright, downwd )
c     =============================================================
c
      integer device, width, fgc, bgc, xstart, xend, ypos
      logical dleft, dright, downwd
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Draws a horizontal line on the host device
      logical dhohor
c
c     Draws a horizontal line on the framestore device
      logical dfshor
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
         devhor = dhohor ( width, fgc, bgc, xstart, xend, ypos,
     +                     dleft, dright, downwd )
      else
         devhor = dfshor ( ) !width, fgc, bgc, xstart, xend, ypos,
!     +                     dleft, dright, downwd )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c     LOGICAL FUNCTION DEVVER ( DEVICE, WIDTH, FGC, BGC, YSTART,
C                               YEND, XPOS, DTOP, DBOT, RITEWD )
c     ----------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device on which line is to be
c                        drawn.
c
c       integer width : INPUT - the width of the line.
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours in which to draw the line.
c
c       integer ystart, yend : INPUT - the start & end positions of the
c                              line (device coordinates)
c
c       integer xpos : INPUT - the x position of the line
c
c       logical dtop, dbot : INPUT - flags indicating if top and bottom
c                            ends of line need horizontals.
c
c       logical ritewd : INPUT - flag indicating direction of
c                        horizontals.
c
c       Draws a verical line on a device.  Special action is taken
c       at ends if needed if it is meeting a horizontal line.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function devver ( device, width, fgc, bgc, ystart, yend,
     +                          xpos, dtop, dbot, ritewd )
c     ================================================================
c
      integer device, width, fgc, bgc, ystart, yend, xpos
      logical dtop, dbot, ritewd
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     CALLED FUNCTIONS:
c
c     Draws a vertical line on the host device
      logical dhover
c
c     Draws a vertical line on the framestore device
      logical dfsver
c
c     Switch on device type
c
      if ( device .eq. HOST ) then
c         write ( 6, 100 ) yend - ystart + 1
c100      format ( ' About to do ', i10, ' vertical chars' )
         devver = dhover ( width, fgc, bgc, ystart, yend, xpos,
     +                     dtop, dbot, ritewd )
      else
         devver = dfsver ( ) !width, fgc, bgc, ystart, yend, xpos,
!    +                     dtop, dbot, ritewd )
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
