C Semper 6 system module SEMSOP
C
      LOGICAL FUNCTION SEMSOP()
C
C Called to signal the start of a fresh page of output, i.e. resets the
C line count of the number of lines output to the terminal.  SEMSOP is
C called in the first place in SEMPRG whenever interactive input is
C requested.  It is also called in a few other places where other types
C of iteractive input take place, e.g. before calls to INKEY, etc.
C
      INCLUDE 'COMMON'
C
      TERCNT=0
C
      SEMSOP=.FALSE.
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
