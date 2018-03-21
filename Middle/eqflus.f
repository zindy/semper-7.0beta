C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQFLUS ( SOURCE )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT - Source to flush.
C
C       Function flushes (empties) the given event queue source.
C
C       Function returns TRUE in case of error, othewise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQFLUS ( SOURCE )
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
C     Deal with the required source.  Set both read and write pointers
C     to zero to empty the queue.
C
      STATUS = .FALSE.
      IF ( SOURCE .EQ. MBREAK ) THEN
C
C        Flush break queue
C
         BRKCNT = 0
      ELSE IF ( SOURCE .EQ. MKEY ) THEN
C
C        Flush keyboard queue
C
         KQINTO = 0
         KQREAD = 0
      ELSE IF ( SOURCE .EQ. MPOINT ) THEN
C
C        Flush pointer queue
C
         PQINTO = 0
         PQREAD = 0
C
         CALL EQXPFL
      ELSE IF ( SOURCE .EQ. MBUT ) THEN
C
C        Flush switches queue
C
         SQINTO = 0
         SQREAD = 0
C
         CALL EQXSFL
      ELSE
C
C        Error - invalid source
C
         STATUS = .TRUE.
      ENDIF
C
C     All done
C
      EQFLUS = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
