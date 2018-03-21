C Semper 6 primitive proving program for MCTIME
C
      PROGRAM MCTIME_TEST
      INTEGER TERM2
C
C ****** CHANGE ******
C TERM2 is the unit number for verification messages
      PARAMETER (TERM2=6)
C ****** ****** ******
C
      INTEGER*2 N(7)
      CALL MCTIME(N)
      WRITE (TERM2,10) N
   10 FORMAT ('   Year Month   Day  Hour   Min   Sec Centisec'
     +   /1X,7I6)
      STOP
C
C Copyright (C) 1987:  Synoptics Ltd,  All Rights Reserved
C
      END
