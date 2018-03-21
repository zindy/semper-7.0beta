C Semper 6 graphics utility routine FSQMON
C
      LOGICAL FUNCTION FSQMON(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX)
C
      INTEGER FRAME,ZOOM
      REAL    XMIN,XMAX,YMIN,YMAX
C
C Returns the frame number, zoom factor and limits currently being
C viewed.  This takes into account all the current viewing conditions,
C and any variations that the particular framestore may impose
C (in particular, any truncation of pan offsets).  The limits are
C returned in graphics coordinates.  The transformation from graphics
C to display coordinates and the default for the view centre must have
C been previously set up in COMMON via a call to FSINIT.
C
      LOGICAL FSVQ61
C
      INTEGER XMMIN,XMMAX,YMMIN,YMMAX
      REAL    X1,X2,Y1,Y2
C
      INCLUDE 'COMMON'
C
      FSQMON=.TRUE.
C
C Fetch current frame and monitor limits
C
      IF (FSVQ61(FRAME,ZOOM,XMMIN,XMMAX,YMMIN,YMMAX,ERROR)) GOTO 10
C
C Determine monitor limits in graphics coordinates (beware of
C transformations that invert the coordinate axes)
C
      X1=(REAL(XMMIN)-FSXOFF)/FSXSCA
      X2=(REAL(XMMAX)-FSXOFF)/FSXSCA
      Y1=(REAL(YMMIN)-FSYOFF)/FSYSCA
      Y2=(REAL(YMMAX)-FSYOFF)/FSYSCA
C
      XMIN=MIN(X1,X2)
      XMAX=MAX(X1,X2)
      YMIN=MIN(Y1,Y2)
      YMAX=MAX(Y1,Y2)
C
      FSQMON=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
