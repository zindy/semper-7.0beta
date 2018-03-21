C Semper 6 system routine SEMSZZ
C
      SUBROUTINE SEMSZZ(NPIC)
      INTEGER NPIC
C
C Sets SELECT to NPIC if 'appropriate', which currently means:
C - unless NPIC on display and CD is not a display
C   (no selection if either medium is undefined)
C
      INCLUDE 'COMMON'
C
      INTEGER DEVICE,MEDIUM
C
      DEVICE = NPIC/1000
      IF (DEVICE.LT.1 .OR. DEVICE.GT.NDVS) GOTO 10
      MEDIUM = MEDN(DEVICE)
      IF (MEDIUM.EQ.0) GOTO 10
      IF (MEDIUM.EQ.MEDDS) THEN
         DEVICE = NINT(CD)
         IF (DEVICE.LT.1.OR.DEVICE.GT.NDVS) GOTO 10
         MEDIUM = MEDN(DEVICE)
         IF (MEDIUM.NE.MEDDS) GOTO 10
      ENDIF
      SELECT=NPIC
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
