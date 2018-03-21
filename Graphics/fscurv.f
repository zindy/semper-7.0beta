C Semper 6 system module FSCURV
C
      LOGICAL FUNCTION FSCURV(X,Y,N,CLOSED)
C
C Outputs series of straight lines joining together N points whose
C positions in graphics coordinates are contained in the arrays X and Y.
C If CLOSED is set to .TRUE., an extra line is drawn to join the last
C point to the first point.
C
      INTEGER N,I
      REAL X(*),Y(*)
      LOGICAL FSLINE,CLOSED
C
      FSCURV=.TRUE.
C
C Join series of points with straight lines
C
      DO 10 I=1,N-1
         IF (FSLINE(X(I),Y(I),X(I+1),Y(I+1))) GOTO 20
   10 CONTINUE
C
C If closed curve, draw line from last point to first point (if any)
C
      IF (CLOSED.AND.N.GE.1) THEN
         IF (FSLINE(X(N),Y(N),X(1),Y(1))) GOTO 20
      ENDIF
C
      FSCURV=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
