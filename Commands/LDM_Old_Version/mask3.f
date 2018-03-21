C Semper 6 subsidiary module MASK3
C
      SUBROUTINE MASK3(X,N)
C
C Sorts N floating-point values in array X.  Sort is done in place
C using straightforward insertion sort which is very efficient for
C sorting a small number of values.
C
      REAL X(*)
      INTEGER N
      REAL Y
      INTEGER I,J
C
      DO 20 I=2,N
C
         J = I-1
         Y = X(I)
C
   10    IF (Y.LT.X(J)) THEN
            X(J+1)=X(J)
            J=J-1
            IF (J.GT.0) GOTO 10
         ENDIF
C
         X(J+1)=Y
C
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
