C Semper 6 system module SEMPPN
C
      INTEGER FUNCTION SEMPPN(N)
      INTEGER N
C
C Adds device number CD to N if N is below 1000
C
      INCLUDE 'COMMON'
C
      INTEGER I,M
C
      I = N
      IF (I .LT. 1000) THEN
         IF (I .NE. 0) THEN
            M = CD
            IF (M .GT. 1000) M = M/1000
            I = I + M*1000
         ENDIF
      ENDIF
      SEMPPN = I
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
