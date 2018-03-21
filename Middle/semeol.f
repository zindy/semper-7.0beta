C Semper 6 system module SEMEOL
C
      LOGICAL FUNCTION SEMEOL()
C
C Generate sequence of carriage-control characters to ensure the
C termination of the current line of output to the terminal
C
      LOGICAL SEMTCR
C
      INCLUDE 'COMMON'
C
      SEMEOL = .TRUE.
C
C If current line not already terminated with end-of-line sequence,
C output carriage-return as end-of-line sequence
      IF (.NOT.TERCR) THEN
         IF (SEMTCR()) GOTO 10
         TERCR = .TRUE.
      ENDIF
C
      SEMEOL = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
