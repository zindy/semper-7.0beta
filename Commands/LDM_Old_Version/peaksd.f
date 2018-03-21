C Semper 6 subsidiary module PEAKSD
C
      SUBROUTINE PEAKSD(X,Y,VALUE,N,BOTH)
C
C Deletes entries in position list contained in arrays X, Y and VALUE
C for which X(i) > 1e5.  If BOTH is .FALSE., only values in X and Y
C are deleted.
C
      INTEGER N
      REAL X(*),Y(*),VALUE(*)
      LOGICAL BOTH
C
      INTEGER I,J
C
C Scan through position list
C
      IF (N .GT. 0) THEN
         J=0
         DO 10 I=1,N
C
C Skip this entry if X(i) > 1e5
C
            IF (X(I).GT.1E5) GOTO 10
C
C Skip this entry if no entries deleted yet
C
            J=J+1
            IF (J.EQ.I) GOTO 10
C
C Move entry to new end position
C
            X(J)=X(I)
            Y(J)=Y(I)
            IF (BOTH) VALUE(J)=VALUE(I)
   10    CONTINUE
C
C Return number of entries left in position list
C
         N=J
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
