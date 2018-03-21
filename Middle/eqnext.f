C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQNEXT ( SOURCE )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer source : OUTPUT - Returns the source with the next
C                                 event
C
C       Function returns the next source for the next event in SOURCE.
C       The next event may not be the next one temporally, since each
C       source is examined in priority order until a non-empty queue is
C       found.  If all queues are empty, function returns TRUE, but will
C       initiate polling on passive devices (e.g. demand mice etc.) for
C       the next call.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQNEXT ( SOURCE )
C     ==================================
C
      INTEGER SOURCE
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Look at each queue.  Priority is Break queue, Keyboard queue,
C     Pointer queue and Switch queue.
C
      STATUS = .TRUE.
C
C     Break queue...
C
      IF ( BRKSTA .EQ. QRUN ) THEN
         IF ( BRKCNT .NE. 0 ) THEN
C
C           Return the break queue as the next source
C
            SOURCE = MBREAK
            STATUS = .FALSE.
            GOTO 10
         ENDIF
      ENDIF
C
C     Keyboard queue...
C
      IF ( KEYSTA .EQ. QRUN ) THEN
         IF ( KQREAD .NE. KQINTO ) THEN
C
C           Return the keyboard queue as the next source
C
            SOURCE = MKEY
            STATUS = .FALSE.
            GOTO 10
         ENDIF
      ENDIF
C
C     Pointer queue...
C
      IF ( PNTSTA .EQ. QRUN ) THEN
         IF ( PQREAD .NE. PQINTO ) THEN
C
C           Return the pointer queue as the next source
C
            SOURCE = MPOINT
            STATUS = .FALSE.
            GOTO 10
         ENDIF
      ENDIF
C
C     Switch queue...
C
      IF ( SWTSTA .EQ. QRUN ) THEN
         IF ( SQREAD .NE. SQINTO ) THEN
C
C           Return the switch queue as the next source
C
            SOURCE = MBUT
            STATUS = .FALSE.
            GOTO 10
         ENDIF
      ENDIF
C
   10 CONTINUE
C
C     Poll the queues if we have not got anything ready
C
      IF ( STATUS ) THEN
         IF ( BRKSTA .EQ. QRUN ) CALL EQXBPL
         IF ( KEYSTA .EQ. QRUN ) CALL EQXKPL
         IF ( PNTSTA .EQ. QRUN ) CALL EQXPPL
         IF ( SWTSTA .EQ. QRUN ) CALL EQXSPL
      ELSE
C
C        Poll higher priority queues
C
         IF ( SOURCE .EQ. MBREAK ) GOTO 20
         IF ( BRKSTA .EQ. QRUN ) CALL EQXBPL
         IF ( SOURCE .EQ. MKEY ) GOTO 20
         IF ( KEYSTA .EQ. QRUN ) CALL EQXKPL
         IF ( SOURCE .EQ. MPOINT ) GOTO 20
         IF ( PNTSTA .EQ. QRUN ) CALL EQXPPL
      ENDIF
C
C     All done
C
   20 EQNEXT = STATUS
      RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
