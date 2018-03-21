C Semper 6 processing module WAITU
C
      SUBROUTINE WAITU
C
C Provides verb WAIT, to manage task suspensions according to the value
C of the FOR key:
C
C     FOR > 0, waits for FOR secs
C     FOR = 0, continues immediately
C
C     FOR < 0, waits for terminal key (or mouse button press)
C
      LOGICAL EQGETD,INKEY,SEMPRO,SEMEOL,SEMSOP
C
      LOGICAL SEMWAI
      REAL VAL
C
      INTEGER NOBUTS,IDUM,KEY
      REAL SECS
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
C     INTEGER IPACK
C
C Fetch value for FOR key
C
      SECS = VAL(10218)
C
C If FOR > 0, wait for the appointed time
C
      IF (SECS .GT. 0.0) THEN
         IF (SEMWAI(SECS)) GOTO 10
C
C If FOR < 0, wait for input
C
      ELSE IF (SECS .LT. 0.0) THEN
C
C See how many mouse buttons we have
C
         IF (EQGETD(MBUT, NOBUTS, IDUM, IDUM)) THEN
            ERROR = 161
            GOTO 10
         ENDIF
C
C Output appropriate prompt
C
         IF (NOBUTS .GT. 0) THEN
            IF (SEMPRO(
     +     'Waiting - press any key or mouse button to continue'))
     +          GOTO 10
         ELSE
            IF (SEMPRO('Waiting - press any key to continue'))
     +          GOTO 10
         ENDIF
C
C Wait for some input
C
         IF (INKEY(KEY, ERROR)) GOTO 10
C
C Indicate end of line
C
         IF (SEMEOL()) GOTO 10
C
C Reset pagination count
C
         IF (SEMSOP()) GOTO 10
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
