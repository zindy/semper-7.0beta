C Semper 6 primitive module WAITS
C
      SUBROUTINE WAITS(SECS)
C
      REAL SECS
C
      INTEGER*4 IMSEC,I
C
      IF (SECS.GT.0.0) THEN
         IMSEC=1000000
         DO 20 I=1,INT(SECS)
            CALL USLEEP(IMSEC)
   20    CONTINUE
C
         IMSEC=1000000.0*(SECS-AINT(SECS))
         CALL USLEEP(IMSEC)
      ENDIF
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
