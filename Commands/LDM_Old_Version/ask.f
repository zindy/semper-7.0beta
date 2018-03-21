C Semper 6 processing module ASK
C---------------------------------------------------------------------
C
C      SUBROUTINE ASK
C      --------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command ASK
C
C---------------------------------------------------------------------
C
      SUBROUTINE ASK
C     ==============
C
      INTEGER IVAL
      LOGICAL SEMASK
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      INTEGER IPTR
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .NE. 0) THEN
C
C ASK processing
C
         IF (SEMASK(LINBUF,COMLIM,IPTR)) GOTO 10
      ENDIF
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
