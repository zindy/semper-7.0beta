C Semper 6 system module SEMDIA
C
      LOGICAL FUNCTION SEMDIA(TEXT,ISEVER)
C
      CHARACTER*(*) TEXT
      INTEGER ISEVER
C
C Outputs a line of text to the diagnostic output stream.  ISEVER
C defines the severity of the diagnostic text:
C
C    ISEVER = NDIMES, informational message
C           = NDIWAR, warning message
C           = NDIERR, error message
C           = NDIFAT, fatal error message
C
      LOGICAL SEMECH
C
      INCLUDE 'PARAMS'
C
      SEMDIA=.TRUE.
C
C Output text to physical output streams enabled for diagnostic output
C
      IF (SEMECH(NOSDIA,TEXT)) GOTO 10
C
      SEMDIA=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      IJUNK = ISEVER
      END
