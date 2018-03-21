C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQSETS ( SOURCE, STATE )
C       -----------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT - Defines the event source whose state
C                        is to be reset
C
C       integer state : INPUT - Defines the new state in which to place
C                       the source
C
C       Function is used to change the actions for an event source.  It
C       will return .TRUE. if SOURCE or STATE are invalid or if any
C       other error occurs.  If the source of the queue is not
C       physically present, the attempting to set the queue into any
C       state will result in the queue being set into QCLOSE state.
C       If the queue is already in the required state, then no action
C       is taken.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQSETS ( SOURCE, STATE )
C
C     =========================================
C
      INTEGER SOURCE
      INTEGER STATE
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
C     Local copy of state flag passed in
C
      INTEGER LOCSTA
C
C     Check state is valid
C
      STATUS = .TRUE.
      IF ( STATE .EQ. QRUN  .OR. STATE .EQ. QWAIT .OR.
     +     STATE .EQ. QOPEN .OR. STATE .EQ. QCLOSE ) THEN
C
C        Now switch on the type of source.
C
         LOCSTA = STATE
         STATUS = .FALSE.
         IF ( SOURCE .EQ. MBREAK ) THEN
C
C           Break queue
C           -----------
C
C           If source not physically there, set queue closed
C
            IF ( .NOT. BRKPHS ) LOCSTA = QCLOSE
C
C           If queue already in the required state, don't change it
C
            IF ( LOCSTA .NE. BRKSTA ) THEN
               IF ( LOCSTA .EQ. QCLOSE .OR. LOCSTA .EQ. QOPEN ) THEN
C
C                 Destroy the queue
C
                  BRKCNT = 0
                  IF ( LOCSTA .EQ. QOPEN ) LOCSTA = QRUN
               ENDIF
C
C              And set the queue state
C
               BRKSTA = LOCSTA
C
C              If opening, poll for input
C
               IF ( BRKSTA .EQ. QRUN ) CALL EQXBPL
            ENDIF
         ELSE IF ( SOURCE .EQ. MKEY ) THEN
C
C           Keyboard queue
C           --------------
C
C           If source not physically there, set queue closed
C
            IF ( .NOT. KBDPHS ) LOCSTA = QCLOSE
C
C           If queue already in the required state, don't change it
C
            IF ( LOCSTA .NE. KEYSTA ) THEN
               IF ( LOCSTA .EQ. QCLOSE .OR. LOCSTA .EQ. QOPEN ) THEN
C
C                 Set read and write pointers to zero to destroy queue
C
                  KQINTO = 0
                  KQREAD = 0
C
C                 And the last character read
C
                  KQLAST = 0
                  IF ( LOCSTA .EQ. QOPEN ) LOCSTA = QRUN
               ENDIF
C
C              And set the queue state
C
               KEYSTA = LOCSTA
C
C              If opening, poll for input
C
               IF ( KEYSTA .EQ. QRUN ) CALL EQXKPL
            ENDIF
         ELSE IF ( SOURCE .EQ. MPOINT ) THEN
C
C           Pointer queue
C           -------------
C
C           If source not physically there, set queue closed
C
            IF ( .NOT. PNTPHS ) LOCSTA = QCLOSE
C
C           If queue already in the required state, don't change it
C
            IF ( LOCSTA .NE. PNTSTA ) THEN
               IF ( PNTSTA .EQ. QCLOSE .OR. LOCSTA .EQ. QOPEN ) THEN
C
C                 Set read and write pointers to zero to destroy queue
C
                  PQINTO = 0
                  PQREAD = 0
C
                  CALL EQXPFL
C
                  IF ( LOCSTA .EQ. QOPEN ) LOCSTA = QRUN
               ENDIF
C
C              And set the queue state
C
               PNTSTA = LOCSTA
C
C              If opening, poll for input
C
               IF ( PNTSTA .EQ. QRUN ) CALL EQXPPL
            ENDIF
         ELSE IF ( SOURCE .EQ. MBUT ) THEN
C
C           Switch queue
C           ------------
C
C           If source not physically there, set queue closed
C
            IF ( .NOT. SWTPHS ) LOCSTA = QCLOSE
C
C           If queue already in the required state, don't change it
C
            IF ( LOCSTA .NE. SWTSTA ) THEN
               IF ( SWTSTA .EQ. QCLOSE .OR. LOCSTA .EQ. QOPEN ) THEN
C
C                 Set read and write pointers to zero to destroy queue
C
                  SQINTO = 0
                  SQREAD = 0
C
                  CALL EQXSFL
C
C                 Set last switch information
C
                  SWTLOP = 0
                  SWTLCL = 0
                  SWTLCS = 0
C
                  IF ( LOCSTA .EQ. QOPEN ) LOCSTA = QRUN
               ENDIF
C
C              And set the queue state
C
               SWTSTA = LOCSTA
C
C              If opening, poll for input
C
               IF ( SWTSTA .EQ. QRUN ) CALL EQXSPL
            ENDIF
         ELSE
C
C           Error - unknown source type
C
            STATUS = .TRUE.
         ENDIF
      ENDIF
C
C     All done
C
      EQSETS = STATUS
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
