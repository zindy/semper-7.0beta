C Semper 6 subsidiary module PEAKSR
C
      SUBROUTINE PEAKSR(X,Y,VALUE,N,ASCEND)
C
C Re-arranges data in arrays X, Y and VALUE by sorting the values in
C array VALUE.  Shell sort algorithm used - O(a*N**1.26)
C Note: The tree sort algorithm (O(N*log(N)) migth be better.
C       Also, sorting with a list of pointers would enable the values
C       in arrays X and Y to be re-arranged after the sort is completed
C
      INTEGER N
      REAL X(*),Y(*),VALUE(*)
      LOGICAL ASCEND
C
      REAL V1,V2,XY
      INTEGER I,IPASS,J,K,M,NPASS
C
C Determine number of passes required
C
      IF (N .GT. 0) THEN
         NPASS = 0
         M = 1
   10    IF (M .LT. N) THEN
            NPASS = NPASS+1
            M = (M+M) + 1
            GOTO 10
         ENDIF
C
C Make required number of passes through data
C
         DO 40 IPASS=1,NPASS
C
C Determine next increment
C
            M=M/2
C
C Sort data with next increment
C
            DO 30 I=1,N-M
C
               DO 20 J=I,1,-M
C
                  K=J+M
C
                  V1=VALUE(J)
                  V2=VALUE(K)
C
                  IF (ASCEND) THEN
                     IF (V1.LE.V2) GOTO 30
                  ELSE
                     IF (V2.LE.V1) GOTO 30
                  ENDIF
C
                  VALUE(K)=V1
                  VALUE(J)=V2
C
                  XY=X(J)
                  X(J)=X(K)
                  X(K)=XY
C
                  XY=Y(J)
                  Y(J)=Y(K)
                  Y(K)=XY
   20          CONTINUE
   30       CONTINUE
   40    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
