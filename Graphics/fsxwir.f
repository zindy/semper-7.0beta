C Semper 6 system module FSXWIR
C
      LOGICAL FUNCTION FSXWIR(X0,Y0,X,Y)
C
      REAL X0,Y0,X,Y
C
C Returns cursor position (X,Y) in graphics coordinates.  The initial
C cursor position is set to (X0,Y0).  If (X0,Y0) is outside the display
C monitor limits, the initial cursor position is set to the default for
C the view centre.  The transformation from graphics to display
C coordinates and the default for the view centre must have been
C previously set up in COMMON via a call to FSINIT.  FSINIT will also
C have determined, in the case of picture coordinates being selected,
C whether input from the real or imaginary part of a complex display
C picture has been requested by means of one of the options RE or IM.
C
      LOGICAL FSQMON,FSXW61,ABANDN
C
      INCLUDE 'COMMON'
C
      INTEGER FRAME,ZOOM,IX,IY
      REAL    XMIN,XMAX,YMIN,YMAX
C
      FSXWIR=.TRUE.
C
C Pick up cursor start position
C
      X=X0
      Y=Y0
C
C If option IM specified, add offset between real and imaginary
C parts
C
      IF (FSI1.EQ.2) X=X+FSIOFF
C
C Fetch current frame and monitor limits
C
      IF (FSQMON(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX)) GOTO 10
C
C If the initial cursor position is outside the monitor limits, set it
C to the centre of the monitor limits
C
      IF (X0.LT.XMIN.OR.X0.GT.XMAX.OR.Y0.LT.YMIN.OR.Y0.GT.YMAX) THEN
         X=(XMIN+XMAX)/2.0
         Y=(YMIN+YMAX)/2.0
      ENDIF
C
C Convert initial cursor position to display coordinates
C
      IX=NINT(FSXSCA*X+FSXOFF)
      IY=NINT(FSYSCA*Y+FSYOFF)
C
C Read cursor position from display
C
      IF (FSXW61(IX,IY,ERROR)) GOTO 10
C
C Check for request to abandon processing
C
      IF (ABANDN(ERROR)) GOTO 10
C
C Convert cursor position from display to graphics coordinates
C
      X=(REAL(IX)-FSXOFF)/FSXSCA
      Y=(REAL(IY)-FSYOFF)/FSYSCA
C
C If option IM specified, remove offset between real and
C imaginary parts
C
      IF (FSI1.EQ.2) X=X-FSIOFF
C
      FSXWIR=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
