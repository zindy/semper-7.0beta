C Semper 6 utility routine PDELTA
C
      LOGICAL FUNCTION PDELTA(DX,DY,N,IEVENT,ICODE)
C
      INTEGER DX(*),DY(*),N,IEVENT,ICODE
C
C Reads the pointer event queue, recording all pointer movements, until
C the pointer event queue is emptied or the dummy arrays DX and DY are
C full, or until a terminating event is encountered (key press or button
C up/down).  PDELTA returns and error if there is no preceding call to
C EVOPEN or if a break event is detected.  Pointer movements are
C returned in arrays DX and DY.  N on entry defines the size of the
C arrays and on exit is set to the number of pointer movements.
C IEVENT is set as follows:
C
C    IEVENT = 0, no terminating event
C           = 1, key pressed, ICODE returns key code
C           = 2, button closed, ICODE returns button number
C           = 3, button opened,   "      "      "      "
C
C PDELTA incorporates support for simulating pointer movement with the
C cursor keys on the keyboard.  This includes the facility to alter the
C pointer step size by means of the 'C' and 'F' keys (coarse/fine).
C
      LOGICAL EQREAD,EQNEXT,DONE
      INTEGER KEY,IOPEN,ICLOSE,IX,IY,IDUM,QUEUE,NMAX
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      PDELTA = .TRUE.
C
C Fault bad array size N
C
      IF (N.LT.1) THEN
         ERROR = 77
         IDMESS = 'Bad array size in call to PDELTA'
         GOTO 20
      ENDIF
C
C Fault absence of call to EVOPEN
C
      IF (.NOT.LEVENT) THEN
         ERROR = 77
         IDMESS = 'EVOPEN not called before PDELTA'
         GOTO 20
      ENDIF
C
      DONE = .FALSE.
C
      NMAX = N
      N = 0
      IEVENT = 0
      ICODE = 0
C
C Interrogate break, keyboard, button and pointer event queues
C
   10 IF (EQNEXT(QUEUE)) THEN
C
C All queues are empty - return with null terminating event
C
         DONE = .TRUE.
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
C Key pressed, return key code as terminating event
C
            IF (EQREAD(MKEY,QTAKE,IDUM,KEY,IDUM,IDUM)) GOTO 30
C
C Process any key codes that affect pointer movement
C
            IF (KEY.EQ.KBLEFT) THEN
               N = N + 1
               DX(N) = -KBSTEP
               DY(N) = 0
            ELSE IF (KEY.EQ.KBRITE) THEN
               N = N + 1
               DX(N) = KBSTEP
               DY(N) = 0
            ELSE IF (KEY.EQ.KBUP) THEN
               N = N + 1
               DX(N) = 0
               DY(N) = KBSTEP
            ELSE IF (KEY.EQ.KBDOWN) THEN
               N = N + 1
               DX(N) = 0
               DY(N) = -KBSTEP
            ELSE IF (KEY.EQ.KUCC.OR.KEY.EQ.KLCC) THEN
               IF (KBSTEP.LT.128) KBSTEP = 2 * KBSTEP
            ELSE IF (KEY.EQ.KUCF.OR.KEY.EQ.KLCF) THEN
               IF (KBSTEP.GT.1) KBSTEP = KBSTEP / 2
            ELSE
               IEVENT = 1
               ICODE = KEY
               DONE = .TRUE.
            ENDIF
         ELSE IF (QUEUE.EQ.MBUT) THEN
C
C Mouse button pressed, return button up/down and number as terminating
C event
C
            IF (EQREAD(MBUT,QTAKE,IDUM,ICLOSE,IOPEN,IDUM)) GOTO 30
            IF (ICLOSE.NE.0) THEN
               IEVENT = 2
               ICODE = ICLOSE
            ENDIF
            IF (IOPEN.NE.0) THEN
               IEVENT = 3
               ICODE = IOPEN
            ENDIF
            DONE = .TRUE.
         ELSE IF (QUEUE.EQ.MPOINT) THEN
C
C Pointer move, add pointer movements to return arrays
C
            IF (EQREAD(MPOINT,QTAKE,IDUM,IX,IY,IDUM)) GOTO 30
            N = N + 1
            DX(N) = IX
            DY(N) = -IY
         ENDIF
      ENDIF
C
C Go back for more?
C
      IF (.NOT.DONE.AND.N.LT.NMAX) GOTO 10
C
      PDELTA = .FALSE.
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
