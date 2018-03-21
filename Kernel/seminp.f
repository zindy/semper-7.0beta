C Semper 6 system module SEMINP
C
      LOGICAL FUNCTION SEMINP(TEXT)
C
      CHARACTER*(*) TEXT
C
C Outputs a line of text to the output stream for non-command input
C
      LOGICAL SEMECH
C
      INCLUDE 'PARAMS'
C
      SEMINP=.TRUE.
C
C Output text to physical output streams enabled for output of
C non-command input
C
      IF (SEMECH(NOSINP,TEXT)) GOTO 10
C
      SEMINP=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
