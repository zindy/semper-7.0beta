C Semper 6 processing module RETUR
C---------------------------------------------------------------------
C
C      SUBROUTINE RETUR
C      ----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command RETURN
C
C---------------------------------------------------------------------
C
      SUBROUTINE RETUR
C     ================
C
      LOGICAL SEMRET
C
      INCLUDE 'COMMON'
C
C RETURN (END) processing
C (END processing occurs when SEMINX traps an END in a program or run
C  file and substitutes RETURN instead).
C
      IF (INPLEV .NE. 0) THEN
         IF (SEMRET(INPLEV-1)) GOTO 10
      ELSE
         IF (INPUT .NE. TERM1) THEN
            IF (SEMRET(0)) GOTO 10
         ENDIF
      ENDIF
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
