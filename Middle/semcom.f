C Semper 6 system module SEMCOM
C
      LOGICAL FUNCTION SEMCOM(TEXT,LENGTH)
C
      CHARACTER*(*) TEXT
      INTEGER LENGTH
C
C Outputs a line of text to the command output stream.  If LENGTH is
C zero, no command text is output.  LENGTH is reset to zero under all
C circumstances.  No output takes place if the current command is ECHO.
C Because the echo settings change during the course of the ECHO
C command, echoing of the ECHO command itself is not clearly defined
C and is best avoided.
C
C     INTEGER IPACK
      LOGICAL SEMECH
C
      INCLUDE 'COMMON'
C
      SEMCOM=.TRUE.
C
C Bypass command reflection if current command is ECHO
C
      IF (VERB.NE.8128) THEN
C
C See if there is anything to output
C
         IF (LENGTH.GT.0) THEN
C
C Output text to physical output streams enabled for command output
C
            IF (SEMECH(NOSCOM,TEXT(1:LENGTH))) GOTO 10
         ENDIF
      ENDIF
C
      SEMCOM=.FALSE.
C
   10 LENGTH=0
C
      RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
