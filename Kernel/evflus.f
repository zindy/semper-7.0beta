C Semper 6 utility routine EVFLUS
C
      LOGICAL FUNCTION EVFLUS(LKEY,LBUT,LPOINT)
C
      LOGICAL LKEY,LBUT,LPOINT
C
C     Flushes specified queues, in preparation for call(s) to
C     other event routines.
C
      LOGICAL EQFLUS
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      EVFLUS = .TRUE.
C
C     Flush keyboard event queue?
C
      IF (LKEY) THEN
         IF ( EQFLUS(MKEY) ) GOTO 20
      ENDIF
C
C     Flush button event queue?
C
      IF (LBUT) THEN
         IF ( EQFLUS(MBUT) ) GOTO 20
      ENDIF
C
C     Flush pointer event queue?
C
      IF (LPOINT) THEN
         IF ( EQFLUS(MPOINT) ) GOTO 20
      ENDIF
C
      EVFLUS = .FALSE.
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
