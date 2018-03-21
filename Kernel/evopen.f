C Semper 6 utility routine EVOPEN
C
      LOGICAL FUNCTION EVOPEN(LKEY,LBUT,LPOINT)
C
      LOGICAL LKEY,LBUT,LPOINT
C
C     Activates specified queues, in preparation for call(s) to
C     other event routines.  Records current state of queues.
C
      LOGICAL EQNQRE,EQSETS
C
      INTEGER IDUM,QSTATE
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      EVOPEN = .TRUE.
C
C If call to EVOPEN not currently active, save current state of queues
C
      IF (.NOT.LEVENT) THEN
         IF (EQNQRE(MKEY,KSTATE,IDUM,IDUM)) GOTO 20
         IF (EQNQRE(MBUT,BSTATE,IDUM,IDUM)) GOTO 20
         IF (EQNQRE(MPOINT,PSTATE,IDUM,IDUM)) GOTO 20
C
C Flag call to EVOPEN
C
         LEVENT = .TRUE.
      ENDIF
C
C Activate/deactivate keyboard event queue
C
      IF (LKEY) THEN
         QSTATE = QRUN
      ELSE
         QSTATE = QCLOSE
      ENDIF
C
      IF (EQSETS(MKEY,QSTATE)) GOTO 20
C
C Activate/deactivate button event queue
C
      IF (LBUT) THEN
         QSTATE = QRUN
      ELSE
         QSTATE = QCLOSE
      ENDIF
C
      IF (EQSETS(MBUT,QSTATE)) GOTO 20
C
C Activate/deactivate pointer event queue
C
      IF (LPOINT) THEN
         QSTATE = QRUN
      ELSE
         QSTATE = QCLOSE
      ENDIF
C
      IF (EQSETS(MPOINT,QSTATE)) GOTO 20
C
      EVOPEN = .FALSE.
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
