C Semper 6 system module SEMLOG
C
      LOGICAL FUNCTION SEMLOG(TEXT)
C
      CHARACTER*(*) TEXT
C
C Outputs a line of text to the log output stream
C
      LOGICAL SEMECH
C
      INCLUDE 'PARAMS'
C
      SEMLOG=.TRUE.
C
C Output text to physical output streams enabled for log output
C
      IF (SEMECH(NOSLOG,TEXT)) GOTO 10
C
      SEMLOG=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
