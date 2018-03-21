C
C Semper 6 primitive routine ABANDN for EVENTS based hosts
C
      LOGICAL FUNCTION ABANDN (IERROR)
C
C     ================================
C
      INTEGER IERROR
      LOGICAL EQNQRE,EQREAD
      LOGICAL ABMATH
C
C     Checks if a break has been signalled via the event queue interface
C
C     It is assumed that the break event queue is already activated.
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER IDUM,QSTATE,LQUEUE,NQUEUE
C
      ABANDN = .TRUE.
C
      CALL SX11CE
C
      IF (EQNQRE(MBREAK, QSTATE, LQUEUE, NQUEUE)) GOTO 20
C
      IF (NQUEUE .NE. 0) THEN
C
C        Clean out break queue
C
         IF (EQREAD(MBREAK, QTAKE, IDUM, IDUM, IDUM, IDUM)) GOTO 20
C
C        Report break
C
         IERROR = 4
         GOTO 10
      ENDIF
C
C     See if arithmetic condition raised...
C
      IF (ABMATH(IERROR)) GOTO 10
C
C     Nothing found to shout about
C
      ABANDN = .FALSE.
C
   10 RETURN
C
C     Report event error
C
   20 IERROR = 161
      GOTO 10
C
C Copyright (C) 1986-1993: Synoptics Ltd, All Rights Reserved
C
      END
