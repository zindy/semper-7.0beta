C Semper 6 system module SEMWAI
C
      LOGICAL FUNCTION SEMWAI(SECS)
C
C Waits for specified period of time, but checks at regular intervals
C for any abandon request
C
      REAL SECS
C
      LOGICAL ABANDN
C
      REAL DSEC
      PARAMETER (DSEC=0.9)
C
      INCLUDE 'COMMON'
C
      REAL ASEC
      INTEGER N
C
C Time interval between checks for abandon requests (needs to be less
C than 1 second - see coding for WAITS)
C
      SEMWAI = .TRUE.
C
      IF (SECS .GT. 0.0) THEN
         IF (SECS .GT. 29000.0) THEN
            N = 29000
            ASEC = DSEC
         ELSE
            N = INT(SECS/DSEC)
            ASEC = SECS - REAL(N)*DSEC
         ENDIF
C
C Check for abandon request
C
   10    IF (ABANDN(ERROR)) GOTO 20
C
C Wait for a short while and then check again
C
         IF (N .GE. 0) THEN
            IF (N .GT. 0) THEN
               CALL WAITS(DSEC)
            ELSE
               CALL WAITS(ASEC)
            ENDIF
C
            N = N - 1
            GOTO 10
         ENDIF
      ENDIF
C
      SEMWAI = .FALSE.
C
   20 CONTINUE
      RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
