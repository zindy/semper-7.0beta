C Semper 6 system module FSCIRC
C
      LOGICAL FUNCTION FSCIRC(XCEN,YCEN,RADIUS)
      REAL XCEN,YCEN,RADIUS
C
C Draw circle centred at (XCEN,YCEN) with radius RADIUS.  Circle is
C generated by call to FSARC with start and finish angles set to
C 0.0 and 2*pi.
C
      LOGICAL FSARC
C
      INCLUDE 'PARAMS'
C
C Draw circle
C
      FSCIRC=FSARC(XCEN,YCEN,RADIUS,0.0,TWOPI)
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
