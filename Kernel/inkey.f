C Semper 6 processing primitive routine INKEY
C
      LOGICAL FUNCTION INKEY(KEY,IERROR)
C
      INTEGER KEY,IERROR
C
C     Returns a key code depending on the key pressed.  Will be either
C     one of the standard keyboard event codes, or if a mouse button is
C     pressed, then a code of KMBUT + the button number
C
C-----------------------
C      LOGICAL EQSETS,EQSETD,EQNQRE,EQREAD,EQNEXT,QKSET,QBSET,LBREAK
C-----------------------
      LOGICAL EQSETS,EQNQRE,EQREAD,EQNEXT,QKSET,QBSET,LBREAK
      INTEGER STATEK,STATEB,IOPEN,ICLOSE,IDUM,QUEUE,CLOBUT
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INKEY = .TRUE.
C
      QKSET = .FALSE.
      QBSET = .FALSE.
      LBREAK = .FALSE.
      CLOBUT = -1
C
      IF (BATCH .NE. 0.0) THEN
         IERROR = 77
         IDMESS = 'INKEY cannot be used in BATCH mode'
         GOTO 30
      ENDIF
C
C     Save keyboard event queue state
C
      IF (EQNQRE(MKEY,STATEK,IDUM,IDUM)) GOTO 40
      IF (EQNQRE(MBUT,STATEB,IDUM,IDUM)) GOTO 40
C
C     Activate keyboard and buttons event queues, and lock the
C     pointer so we can get button clicks
C
      IF (EQSETS(MKEY,QRUN)) GOTO 40
      QKSET = .TRUE.
      IF (EQSETS(MBUT,QRUN)) GOTO 40
      QBSET = .TRUE.
C-----------------------
C      IF (EQSETD(MPOINT,OSETL,1,0,0)) GOTO 40
C-----------------------
C
C     Interrogate break, keyboard and buttons event queue
C
   10 CONTINUE
      IF ( EQNEXT ( QUEUE ) ) THEN
         CALL WAITS(0.02)
         GOTO 10
      ELSE
         IF ( QUEUE .EQ. MBREAK ) THEN
C
C           Break detected, clean out break queue and flag break
C
            IF (EQREAD(MBREAK,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 40
            IERROR = 4
            LBREAK = .TRUE.
            GOTO 20
         ELSE IF ( QUEUE .EQ. MKEY ) THEN
C
C           Key pressed, fetch key code
C
            IF (EQREAD(MKEY,QTAKE,IDUM,KEY,IDUM,IDUM)) GOTO 40
            GOTO 20
         ELSE IF ( QUEUE .EQ. MBUT ) THEN
C
C           Mouse button pressed, fetch set suitable key code
C           Note we wait for the button close then open.
C
            IF (EQREAD(MBUT,QTAKE,IDUM,ICLOSE,IOPEN,IDUM)) GOTO 40
            IF ( ICLOSE .NE. 0 ) THEN
               CLOBUT = ICLOSE
            ELSE IF ( IOPEN .EQ. CLOBUT ) THEN
               KEY = KMBUT + IOPEN
               GOTO 20
            ENDIF
            GOTO 10
         ELSE
C
C           What queue was this then?
C
            GOTO 10
         ENDIF
      ENDIF
   20 CONTINUE
C
C     Restore keyboard and buttons event queue state and unlock
C     the pointer
C
      QKSET = .FALSE.
      IF (EQSETS(MKEY,STATEK)) GOTO 40
      QBSET = .FALSE.
      IF (EQSETS(MBUT,STATEB)) GOTO 40
C-----------------------
C      IF (EQSETD(MPOINT,OSETL,0,0,0)) GOTO 40
C-----------------------
C
      INKEY = LBREAK
C
   30 RETURN
C
C     Events error
C
   40 IERROR = 161
C
      IF (QKSET) THEN
         IF (EQSETS(MKEY,STATEK)) GOTO 30
      ENDIF
      IF (QBSET) THEN
         IF (EQSETS(MBUT,STATEB)) GOTO 30
C-----------------------
C         IF (EQSETD(MPOINT,OSETL,0,0,0)) GOTO 30
C-----------------------
      ENDIF
C
      GOTO 30
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
