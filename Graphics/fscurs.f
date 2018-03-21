C Semper 6 graphics utility routine FSCURS
C
      LOGICAL FUNCTION FSCURS(IOP,X,Y)
C
      INTEGER IOP
      REAL    X,Y
C
C Controls display and positioning of the cursor.  IOP specifies the
C state of the cursor:
C
C    IOP = 1, switch on cursor at position (X,Y)
C        = 2, move cursor to position (X,Y)
C        = 3, switch off cursor at position (X,Y)
C
C The cursor position is set to (X,Y) in graphics coordinates.  If (X,Y)
C is outside the monitor limits, the cursor position is truncated.  The
C cursor is always displayed on the currently viewed frame.  The
C transformation from graphics to display coordinates and the default
C for the view centre must have been previously set up in COMMON via a
C call to FSINIT.  FSINIT will also have determined, in the case of
C picture coordinates being selected, whether input from the real or
C imaginary part of a complex display picture has been requested by
C means of one of the options RE or IM.
C
      LOGICAL FSQMON,FSCC61
C
      INTEGER IX,IY,FRAME,ZOOM
      REAL    XMIN,XMAX,YMIN,YMAX,CX,CY
C
      INCLUDE 'COMMON'
C
      FSCURS=.TRUE.
C
C Pick up new cursor position
C
      CX=X
      CY=Y
C
C If option IM specified, add offset between real and imaginary
C parts
C
      IF (FSI1.EQ.2) CX=CX+FSIOFF
C
C Fetch current frame and monitor limits
C
      IF (FSQMON(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX)) GOTO 10
C
C Truncate cursor position at monitor limits
C
      CX=MIN(MAX(XMIN,CX),XMAX)
      CY=MIN(MAX(YMIN,CY),YMAX)
C
C Convert cursor position to display coordinates
C
      IX=NINT(FSXSCA*CX+FSXOFF)
      IY=NINT(FSYSCA*CY+FSYOFF)
C
C Switch/position cursor
C
      IF (FSCC61(IOP,FRAME,IX,IY,ERROR)) GOTO 10
C
C If option IM specified, remove offset between real and imaginary
C parts
C
      IF (FSI1.EQ.2) CX=CX-FSIOFF
C
C Return final (truncated) cursor position
C
      X=CX
      Y=CY
C
      FSCURS=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
