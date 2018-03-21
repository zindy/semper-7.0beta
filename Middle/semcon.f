C Semper 6 system module SEMCON
C
      LOGICAL FUNCTION SEMCON(TEXT)
C
      CHARACTER*(*) TEXT
C
C Outputs a line of text to the console output stream
C
      LOGICAL SEMECH
C
      INCLUDE 'PARAMS'
C
      SEMCON=.TRUE.
C
C Output text to physical output streams enabled for console output
C
      IF (SEMECH(NOSCON,TEXT)) GOTO 10
C
      SEMCON=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
