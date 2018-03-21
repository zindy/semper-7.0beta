C Semper 6 utility routine KREAD
C
      LOGICAL FUNCTION KREAD(KEY)
C
      INTEGER KEY
C
C Returns a key code depending on the key pressed.  Will be either
C one of the standard keyboard event codes, or if a mouse button is
C pressed, then a code of KMBUT + the button number.  Does not wait
C for input and returns KBNONE if keyboard and button queues are empty.
C
      LOGICAL EQREAD,EQNEXT,DONE
      INTEGER IOPEN,ICLOSE,IDUM,QUEUE,BUTTON
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      KREAD = .TRUE.
C
C Fault absence of call to EVOPEN
C
      IF (.NOT.LEVENT) THEN
         ERROR=77
         IDMESS = 'EVOPEN not called before KREAD'
         GOTO 20
      ENDIF
C
      DONE = .FALSE.
      BUTTON = -1
C
C Interrogate break, keyboard and buttons event queues
C
   10 IF (EQNEXT(QUEUE)) THEN
C
C All queues are empty - if waiting for button to be released, wait
C for short time, otherwise, return KBNONE = no input.
C
         IF (BUTTON.NE.-1) THEN
            CALL WAITS(0.02)
         ELSE
            KEY = KBNONE
            DONE = .TRUE.
         ENDIF
      ELSE
         IF (QUEUE.EQ.MBREAK) THEN
C
C Break detected, clean out break queue
C
            IF (EQREAD(MBREAK,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 30
C
C Flag break as Semper error 4
C
            ERROR = 4
            GOTO 20
         ELSE IF (QUEUE.EQ.MKEY) THEN
C
C Key pressed, fetch key code
C
            IF (EQREAD(MKEY,QTAKE,IDUM,KEY,IDUM,IDUM)) GOTO 30
            DONE = .TRUE.
         ELSE IF (QUEUE.EQ.MBUT) THEN
C
C Mouse button pressed, read event queue and set equivalent key code
C Note we wait for the button close then open.
C
            IF (EQREAD(MBUT,QTAKE,IDUM,ICLOSE,IOPEN,IDUM)) GOTO 30
            IF (ICLOSE.NE.0.AND.BUTTON.EQ.-1) THEN
               BUTTON = ICLOSE
            ELSE IF (IOPEN.EQ.BUTTON) THEN
               KEY = KMBUT + BUTTON
               DONE = .TRUE.
            ENDIF
         ELSE
C
C Some other queueu (pointer?) - remove entry
C
            IF (EQREAD(QUEUE,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 30
         ENDIF
      ENDIF
C
C Go back for more?
C
      IF (.NOT.DONE) GOTO 10
C
      KREAD = .FALSE.
C
   20 RETURN
C
C Events error
C
   30 ERROR = 161
      GOTO 20
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
