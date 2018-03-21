C Semper 6 utility routine EVCLOS
C
      LOGICAL FUNCTION EVCLOS()
C
C     Restores event queues to state before first of current series of
C     calls to EVOPEN
C
      LOGICAL EQSETS
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      EVCLOS = .TRUE.
C
C If call to EVOPEN currently active, restore state of queues
C
      IF (LEVENT) THEN
         IF (EQSETS(MKEY,KSTATE)) GOTO 20
         IF (EQSETS(MBUT,BSTATE)) GOTO 20
         IF (EQSETS(MPOINT,PSTATE)) GOTO 20
C
C Reset flag
C
         LEVENT = .FALSE.
      ENDIF
C
      EVCLOS = .FALSE.
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
