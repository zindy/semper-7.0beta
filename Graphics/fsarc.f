C Semper 6 system module FSARC
C
      LOGICAL FUNCTION FSARC(XCEN,YCEN,RADIUS,THETA1,THETA2)
C
C Draws circular arc centred on (XCEN,YCEN) and radius RADIUS in an
C anticlockwise direction from angular position THETA1 to THETA2.
C The arc is generated as a series of straight lines so that the
C maximum departure from a true arc never exceeds half a pixel unit.
C
      REAL XCEN,YCEN,RADIUS,THETA1,THETA2
C
      LOGICAL FSLINE
C
      INCLUDE 'COMMON'
C
      REAL RTOL
      PARAMETER (RTOL=0.5)
C
      REAL R,DTHETA,THETA,X1,X2,Y1,Y2
      INTEGER I,N
C
      FSARC=.TRUE.
C
C Determine radius in pixel units
C
      R=ABS(FSXSCA)*RADIUS
C
C If radius is less than radial tolerance, output single pixel at arc
C centre position
C
      IF (R.LT.RTOL) THEN
         IF (FSLINE(XCEN,YCEN,XCEN,YCEN)) GOTO 30
C
C Otherwise, generate series of straight lines to represent the arc
C
      ELSE
C
C Determine positive angular difference between start and end of arc
C
         DTHETA=THETA2-THETA1
   10    IF (DTHETA.LT.0.0) THEN
            DTHETA=DTHETA+TWOPI
            GOTO 10
         ENDIF
C
C Determine number of straight line segments required, rounding up to
C next multiple of 4.
C Note: approx. formula for angular increment in radians is: sqrt(8*t/r)
C       where t = maximum departure from true arc in pixel units
C             r = radius of arc in pixel units
C
         N=4*((MAX(INT(DTHETA/SQRT(8.0*RTOL/R)),1)+3)/4)
C
C Set up start angle and angular increment
C
         THETA=THETA1
         DTHETA=DTHETA/REAL(N)
C
C Determine position of start point of arc
C
         X1=XCEN+RADIUS*COS(THETA)
         Y1=YCEN+RADIUS*SIN(THETA)
C
C Output arc as series of straight line segments
C
         DO 20 I=1,N
C
C Determine end point for current line segment
C
            THETA=THETA+DTHETA
C
            X2=XCEN+RADIUS*COS(THETA)
            Y2=YCEN+RADIUS*SIN(THETA)
C
C Output line segment
C
            IF (FSLINE(X1,Y1,X2,Y2)) GOTO 30
C
C Make the current end point the start point for the next line segment
C
            X1=X2
            Y1=Y2
   20    CONTINUE
      ENDIF
C
      FSARC=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
