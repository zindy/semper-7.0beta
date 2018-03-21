C Semper 6 system routine SEMTFC
C
      LOGICAL FUNCTION SEMTFC(LPN,HPL)
C
C Tests whether centre position legal for FOURIER picture, and
C sets HPL to indicate half-plane data
C
      INTEGER LPN
      LOGICAL HPL
C
      INCLUDE 'COMMON'
C
      SEMTFC=.FALSE.
      IF (CROWN(LPN).NE.NROWS(LPN)/2+1) GOTO 10
      IF (CCOLN(LPN).EQ.NCOLS(LPN)/2+1) THEN
         HPL=.FALSE.
      ELSE IF (CCOLN(LPN).EQ.1) THEN
         HPL=.TRUE.
      ELSE
         GOTO 10
      ENDIF
      GOTO 20
C
   10 ERROR=63
      IDERR=1000*DEVN(LPN)+PICN(LPN)
      SEMTFC=.TRUE.
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
