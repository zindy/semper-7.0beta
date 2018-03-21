C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQNQRE ( SOURCE, STATE, LQUEUE, NQUEUE )
C       ---------------------------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT - Defines the event source whose state
C                        is to be interrogated.
C
C       integer state : OUTPUT - Returns the state of the source
C
C       integer lqueue : OUTPUT - Returns the maximum length of the
C                        source
C
C       integer nqueue : OUTPUT - Returns the current length of the
C                        source
C
C       Function returns the current state of the queue in STATE.  This
C       may not be the same as the last state set; a source that is
C       physically not present will always return the state QCLOSE.
C       STATE will never be returned as QOPEN.  LQUEUE is used to return
C       the maximum length of the queue (this will be zero if the source
C       is not present).  NQUEUE returns the current length of the
C       queue.  If NQUEUE and LQUEUE have the same length, it is
C       possible that data may have been lost.  EQNQRE returns .TRUE.
C       if any argument is in error.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQNQRE ( SOURCE, STATE, LQUEUE, NQUEUE )
C     =========================================================
C
      INTEGER SOURCE
      INTEGER STATE
      INTEGER LQUEUE
      INTEGER NQUEUE
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
      LOGICAL STATUS
C
C     Now switch on the type of source, and set the state info.
C
      STATUS = .FALSE.
      IF ( SOURCE .EQ. MBREAK ) THEN
         STATE = BRKSTA
         LQUEUE = 1
C
C        Say queue is full if there are any breaks outstanding
C        and queue physically there
C
         IF ( BRKCNT .GT. 0 ) THEN
            NQUEUE = 1
         ELSE
            NQUEUE = 0
C
C           Poll for next time
C
C            CALL EQXBPL
C-------------------------------------
         ENDIF
      ELSE
         IF ( SOURCE .EQ. MKEY ) THEN
            STATE = KEYSTA
            LQUEUE = KQMASK
C
C           Set the current number of entries
C
            IF ( STATE .NE. QCLOSE ) THEN
               NQUEUE = KQINTO + KQSIZE - KQREAD
               NQUEUE = IAND ( NQUEUE, KQMASK )
C
C              Poll for next time
C
C               CALL EQXKPL
C-------------------------------------
            ELSE
               NQUEUE = 0
            ENDIF
         ELSE
            IF ( SOURCE .EQ. MPOINT ) THEN
               STATE = PNTSTA
               LQUEUE = PQMASK
C
C              Set the current number of entries
C
               IF ( STATE .NE. QCLOSE ) THEN
                  NQUEUE = PQINTO + PQSIZE - PQREAD
                  NQUEUE = IAND ( NQUEUE, PQMASK )
C
C                 Poll for next time
C
C                  CALL EQXPPL
C-------------------------------------
               ELSE
                  NQUEUE = 0
               ENDIF
            ELSE
               IF ( SOURCE .EQ. MBUT ) THEN
                  STATE = SWTSTA
                  LQUEUE = SQMASK
C
C                 Set the current number of entries
C
                  IF ( STATE .NE. QCLOSE ) THEN
                     NQUEUE = SQINTO + SQSIZE - SQREAD
                     NQUEUE = IAND ( NQUEUE, SQMASK )
C
C                    Poll for next time
C
C                     CALL EQXSPL
C-------------------------------------
                  ELSE
                     NQUEUE = 0
                  ENDIF
               ELSE
                  STATUS = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     All done
C
      EQNQRE = STATUS
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
