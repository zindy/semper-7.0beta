
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DFSDSI ( XMIN, YMIN, XSIZE, YSIZE, MXSCA,
c                                 MYSCA, XOFF, YOFF, NCOLS )
c       ----------------------------------------------------------
c
c       PARAMETERS:
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
c       integer ncols : OUTPUT - The number of colours supported.
c
c       Returns the minimum coordiates, size, mouse scale, device
c       offset and number of colours of the framestore display device.
c
c       Function returns TRUE in case of error, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function dfsdsi ( xmin, ymin, xsize, ysize, mxsca, mysca,
     +                          xoff, yoff, ncols )
c     ==================================================================
c
      integer xmin, ymin, xsize, ysize, mxsca, mysca, xoff, yoff, ncols
c
c     Set the device dimensions for caller
c
      xmin = 1
      ymin = 1
      xsize = 80
      ysize = 25
c
c     And the mouse scaling factor
c
      mxsca = 1
      mysca = 1
c
c     And the device offset
c
      xoff = 0
      yoff = 0
c
c     And the number of colours
c
      ncols = 8
c
      dfsdsi = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DFSDST ( DUMMY )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer dummy : INPUT - Dummy argument.
c
c       Intialises the framestore display device.
c
c       Function returns TRUE in case of error, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function dfsdst ( dummy )
c     =================================
c
      integer dummy
c
      dfsdst = .FALSE.
      return
      IJUNK = DUMMY
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DFSDSH ( DUMMY )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer dummy : INPUT - dummy argument.
c
c       Terminates use of the framestore device.
c
c       Function returns TRUE in case of error, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function dfsdsh ( dummy )
c     =================================
c
      integer dummy
c
      dfsdsh = .FALSE.
c
      return
      IJUNK = DUMMY
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCSA
c       -----------------
c
c       PARAMETERS:
c
c       None.
c
c       Saves the state (position and style) of the cursor on the
c       framestore display device.
c
c----------------------------------------------------------------------
c
      subroutine dfscsa
c     =================
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCON
c       -----------------
c
c       PARAMETERS:
c
c       None.
c
c       Makes the cursor on the framestore display device visible.
c
c----------------------------------------------------------------------
c
      subroutine dfscon
c     =================
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCOF
c       -----------------
c
c       PARAMETERS:
c
c       None.
c
c       Makes the cursor on the framestore display device invisible.
c
c----------------------------------------------------------------------
c
      subroutine dfscof
c     =================
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCMO ( XPOS, YPOS )
c       --------------------------------
c
c       PARAMETERS:
c
c       integer xpos, ypos : INPUT - the position (device coordinates)
c                            to which the cursor is to be moved.
c
c       Moves the cursor to the given position on the framestore display
c       device.
c
c----------------------------------------------------------------------
c
      subroutine dfscmo ( xpos, ypos )
c     ================================
c
      integer xpos, ypos
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      IDUMMY = xpos
      IDUMMY = ypos
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCQU ( XPOS, YPOS )
c       --------------------------------
c
c       PARAMETERS:
c
c       integer xpos, ypos : OUTPUT - the position (device coordinates)
c                            at which the cursor is currently
c
c       Returns the current position of the cursor on the framestore
c       display device.
c
c----------------------------------------------------------------------
c
      subroutine dfscqu ( xpos, ypos )
c     ================================
c
      integer xpos, ypos
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      IDUMMY = xpos
      IDUMMY = ypos
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE DFSCRE
c       -----------------
c
c       PARAMETERS:
c
c       None.
c
c       Restores the state (position and style) of the cursor on the
c       framestore display device.
c
c----------------------------------------------------------------------
      subroutine dfscre
c     =================
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DFSTEX ( XPOS, YPOS, STRING, LENGTH,
c                                 FGC, BGC, STYLE )
c       -------------------------------------------------------------
c
c       PARAMETERS:
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
c       and style on the framestore display device.
c
c----------------------------------------------------------------------
c
      logical function dfstex () ! xpos,ypos,string,length,
!    +                         fgc,bgc,style)
c     ========================================================
c
!     integer xpos,ypos
!      character*(*) string
!     integer length, fgc, bgc, style
c
      dfstex = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION DFSCLR ( COLOUR, XPOS, YPOS, XSIZE, YSIZE )
c       ------------------------------------------------------------
c
c       PARAMETERS:
c
c       integer colour : INPUT - the colour to which the area should
c                        be cleared.
c
c       integer xpos, ypos : INPUT - the position (device coordinates)
c                            of the top left of the area to be cleared.
c
c       integer xsize, ysize : INPUT - the size of the area to clear.
c
c       Clears an area of the framestore display device to a given
c       colour.
c
c----------------------------------------------------------------------
c
      logical function dfsclr ( ) ! colour, xpos, ypos, xsize, ysize )
c     ============================================================
c
!     integer colour, xpos, ypos, xsize, ysize
c
      dfsclr = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c     LOGICAL FUNCTION DFSHOR ( WIDTH, FGC, BGC, XSTART, XEND, YPOS,
c                               DLEFT, DRIGHT, DOWNWD )
c     --------------------------------------------------------------
c
c       PARAMETERS:
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
c       Draws a horizontal line on the framestore.  Special action is
c       taken at ends if needed if it is meeting a vertical line.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function dfshor () ! width, fgc, bgc, xstart, xend, ypos,
!    +                          dleft, dright, downwd )
c     ==============================================================
c
!     integer width, fgc, bgc, xstart, xend, ypos
!     logical dleft, dright, downwd
c
      dfshor = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c     LOGICAL FUNCTION DFSVER ( WIDTH, FGC, BGC, YSTART, YEND, XPOS,
c                               DTOP, DBOT, RITEWD )
c     --------------------------------------------------------------
c
c       PARAMETERS:
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
c       logical dtop, dbot : INPUT - flags indicating if top and
c                            bottom ends of line need horizontals.
c
c       logical ritewd : INPUT - flag indicating direction of
c                        horizontals.
c
c       Draws a vertical line on the framestore device.  Special action
c       is taken at ends if needed if it is meeting a horizontal line.
c
c       Function returns TRUE in case of failure, otherwise FALSE
c
c----------------------------------------------------------------------
c
      logical function dfsver ( ) !width, fgc, bgc, ystart, yend, xpos,
!    +                          dtop, dbot, ritewd )
c     ==============================================================
c
!     integer width, fgc, bgc, ystart, yend, xpos
!     logical dtop, dbot, ritewd
c
      dfsver = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
