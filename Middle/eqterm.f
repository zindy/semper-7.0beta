C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQTERM ( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy argument.
C
C       EQTERM is used to shut down all the queues.  All queues are
C       physically closed.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQTERM ( DUMMY )
C     =================================
C
      INTEGER DUMMY
C
      INCLUDE 'EQCOM'
C
C     local variables:
C
C     Return status
C
      LOGICAL STATUS
C
C     CALLED FUNCTIONS:
C
C     Closes the break queue
C
      LOGICAL EQXBCL
C
C     Closes the keyboard queue
C
      LOGICAL EQXKCL
C
C     Closes the pointer queue
C
      LOGICAL EQXPCL
C
C     Closes the switches queue
C
      LOGICAL EQXSCL
C
C     Physically close the queue sources
C
      STATUS = .FALSE.
      IF ( BRKPHS ) STATUS = EQXBCL ( 1 )
      IF ( .NOT. STATUS ) THEN
         IF ( KBDPHS ) STATUS = EQXKCL ( 1 )
         IF ( .NOT. STATUS ) THEN
            IF ( PNTPHS ) THEN
               STATUS = EQXPCL ( 1 )
            ENDIF
            IF ( .NOT. STATUS ) THEN
               IF ( SWTPHS ) THEN
                  STATUS = EQXSCL ( 1 )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     All done
C
      EQTERM = STATUS
C
      RETURN
      IDUMMY = DUMMY
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
