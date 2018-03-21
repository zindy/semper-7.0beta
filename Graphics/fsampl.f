C Semper 6 system module FSAMPL
C
      LOGICAL FUNCTION FSAMPL(XMIN,XMAX,YMIN,YMAX,X,DX,NCOL,Y,DY,NROW)
C
C Given a region defined in graphics coordinates XMIN,XMAX,YMIN,YMAX
C returns the framestore pixel locations within the region, also in
C graphics coordinates.  All elements in the region can then be accessed
C at their precise locations via the graphics coordinates XX,YY below:
C
C       IF (FSAMPL(XMIN,XMAX,YMIN,YMAX,X,DX,NCOL,Y,DY,NROW)) RETURN
C
C       YY=Y
C       DO 20 J=1,NROW
C          XX=X
C          DO 10 I=1,NCOL
C             access pixel with graphics coordinates XX,YY
C             XX=XX+DX
C    10    CONTINUE
C          YY=YY+DY
C    20 CONTINUE
C
C The above loop is guaranted to access the region from LEFT to RIGHT
C and TOP to BOTTOM
C
C FSINIT must be called first
C
C FSAMPL returns .TRUE. if NCOL or NROW exceeds 30000.
C The routine is tolerant of XMIN > XMAX and YMIN > YMAX
C
C If no points lie in the region NCOL or NROW is returned as 0
C
      REAL XMIN,XMAX,YMIN,YMAX,X,DX,Y,DY
      INTEGER NROW,NCOL
C
      LOGICAL FSQTRA
C
      REAL X1,X2,Y1,Y2,AX1,AX2,AY1,AY2,SCALEX,SCALEY,XOFF,YOFF
C
      INCLUDE 'COMMON'
C
      FSAMPL = .TRUE.
C
C Fetch parameters defining the transformation from graphics to
C display coordinates
C
      IF (FSQTRA(SCALEX,XOFF,SCALEY,YOFF)) GOTO 10
C
C Convert to display coordinates
C
      AX1 = XMIN*SCALEX + XOFF
      AX2 = XMAX*SCALEX + XOFF
      AY1 = YMIN*SCALEY + YOFF
      AY2 = YMAX*SCALEY + YOFF
C
      X1 = MIN(AX1,AX2)
      X2 = MAX(AX1,AX2)
      Y1 = MIN(AY1,AY2)
      Y2 = MAX(AY1,AY2)
C
C Integer pixels corresponding to edges of region
C
      AX1 = AINT(X1)
      AX2 = AINT(X2)
      AY1 = AINT(Y1)
      AY2 = AINT(Y2)
C
C Ensure AX1..AY2 lie INSIDE the region or exactly ON the border
C
      IF (AX1 .LT. X1) AX1 = AX1+1.0
      IF (AX2 .GT. X2) AX2 = AX2-1.0
      IF (AY1 .LT. Y1) AY1 = AY1+1.0
      IF (AY2 .GT. Y2) AY2 = AY2-1.0
C
C Test region too large
C
      IF (AX2-AX1 .GT. 3.0E4 .OR. AY2-AY1 .GT. 3.0E4) THEN
         ERROR=89
         GOTO 10
      ENDIF
C
C Number of X,Y steps
C Note: ncol/nrow will be zero if no points lie within the
C       vertical/horizontal bounds
C
      NCOL = NINT(AX2-AX1)+1
      NROW = NINT(AY2-AY1)+1
C
C Calculate step size and direction
C
      DX = 1.0/SCALEX
      DY = 1.0/SCALEY
C
C Return graphics coords of top left corner
C
      X = (AX1-XOFF)/SCALEX
      Y = (AY1-YOFF)/SCALEY
C
      FSAMPL = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
