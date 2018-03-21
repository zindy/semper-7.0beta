C Semper 6 utility routine KPRESS
C
      LOGICAL FUNCTION KPRESS(KEY)
C
      INTEGER KEY
C
C Returns a key code depending on the key pressed.  Will be either
C one of the standard keyboard event codes, or if a mouse button is
C pressed, then a code of KMBUT + the button number.
C
      LOGICAL EQSETS,EQNQRE,EQREAD,EQNEXT,QKSET,QBSET,LBREAK,DONE
      INTEGER STATEK,STATEB,IOPEN,ICLOSE,IDUM,QUEUE,BUTTON
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      KPRESS = .TRUE.
C
      QKSET = .FALSE.
      QBSET = .FALSE.
      LBREAK = .FALSE.
      DONE = .FALSE.
      BUTTON = -1
C
C Save keyboard event queue state
C
      IF (EQNQRE(MKEY,STATEK,IDUM,IDUM)) GOTO 30
      IF (EQNQRE(MBUT,STATEB,IDUM,IDUM)) GOTO 30
C
C Activate keyboard and buttons event queues
C
      IF (EQSETS(MKEY,QRUN)) GOTO 30
      QKSET = .TRUE.
      IF (EQSETS(MBUT,QRUN)) GOTO 30
      QBSET = .TRUE.
C
C Interrogate break, keyboard and buttons event queues
C
   10 IF (EQNEXT(QUEUE)) THEN
C
C All queues are empty - try again after short wait
C
         CALL WAITS(0.02)
      ELSE
         IF (QUEUE.EQ.MBREAK) THEN
C
C Break detected, clean out break queue and flag break
C
            IF (EQREAD(MBREAK,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 30
            ERROR = 4
            LBREAK = .TRUE.
         ELSE IF (QUEUE.EQ.MKEY) THEN
C
C Key pressed, fetch key code
C
            IF (EQREAD(MKEY,QTAKE,IDUM,KEY,IDUM,IDUM)) GOTO 30
            DONE = .TRUE.
         ELSE IF (QUEUE.EQ.MBUT) THEN
C
C Mouse button pressed, fetch set suitable key code
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
C Some other queue (pointer?) - take entry and throw away
C
            IF (EQREAD(QUEUE,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 30
         ENDIF
      ENDIF
C
C Go back for more?
C
      IF (.NOT.(DONE.OR.LBREAK)) GOTO 10
C
C Restore keyboard and buttons event queue state
C
      QKSET = .FALSE.
      IF (EQSETS(MKEY,STATEK)) GOTO 30
      QBSET = .FALSE.
      IF (EQSETS(MBUT,STATEB)) GOTO 30
C
      KPRESS = LBREAK
C
   20 RETURN
C
C Events error
C
   30 ERROR = 161
C
      IF (QKSET) THEN
         IF (EQSETS(MKEY,STATEK)) GOTO 20
      ENDIF
      IF (QBSET) THEN
         IF (EQSETS(MBUT,STATEB)) GOTO 20
      ENDIF
C
      GOTO 20
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
