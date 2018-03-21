C Semper 6 utility routine EVQENQ
C
      LOGICAL FUNCTION EVQENQ(LKEY,LBUT,LPOINT)
C
      LOGICAL LKEY,LBUT,LPOINT
C
C Returns which of the keyboard, button and pointer event queues are
C active
C
      LOGICAL EQNQRE
C
      INTEGER IDUM,QSTATE
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      EVQENQ = .TRUE.
C
C See if keyboard queue is active
C
      IF (EQNQRE(MKEY,QSTATE,IDUM,IDUM)) GOTO 20
      LKEY = QSTATE.EQ.QRUN
C
C See if button queue is active
C
      IF (EQNQRE(MBUT,QSTATE,IDUM,IDUM)) GOTO 20
      LBUT = QSTATE.EQ.QRUN
C
C See if pointer queue is active
C
      IF (EQNQRE(MPOINT,QSTATE,IDUM,IDUM)) GOTO 20
      LPOINT = QSTATE.EQ.QRUN
C
      EVQENQ = .FALSE.
C
   10 RETURN
C
C Event error
C
   20 ERROR = 161
      GOTO 10
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
