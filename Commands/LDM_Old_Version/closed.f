C Semper 6 processing module CLOSED
C---------------------------------------------------------------------
C
C      SUBROUTINE CLOSED
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Force closing of display device
C
C---------------------------------------------------------------------
C
      SUBROUTINE CLOSED
C     =================
C
      INCLUDE 'COMMON'
C
      LOGICAL FSFL61,SEMDEL
C
      INTEGER SAVERR,N
C
      SAVERR = 0
C
      IF (MEDN(1) .EQ. MEDDS) THEN
         IF (FSFL61(2,ERROR)) SAVERR = ERROR
      ENDIF
C
C Close all displays (force device 1 pro tem)
C
      DO 10 N = 1,NDPDS
         IF (SEMDEL(1,1000+N)) GOTO 20
   10 CONTINUE
C
   20 IF (ERROR .EQ. 0 .AND. SAVERR .NE. 0) ERROR = SAVERR
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
