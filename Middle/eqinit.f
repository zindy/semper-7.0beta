C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQINIT ( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy argument.
C
C       EQINIT is used to initialise all the queues.  All queues are
C       set into QCLOSE state.  All the physical sources are queried to
C       check they are physically there, and internal flags set
C       appropriately.  All queues are physically opened.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQINIT ( DUMMY )
C
C     =================================
C
      INTEGER DUMMY
C
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     CALLED FUNCTIONS:
C
C     Opens the break queue
C
      LOGICAL EQXBOP
C
C     Opens the keyboard queue
C
      LOGICAL EQXKOP
C
C     Opens the pointer queue
C
      LOGICAL EQXPOP
C
C     Opens the switches queue
C
      LOGICAL EQXSOP
C
C Initialise COMMON variables left undefined by underlying event code
C
      BQDIS = .FALSE.
      KQDIS = .FALSE.
      PQDIS = .FALSE.
      SQDIS = .FALSE.
C
C     Call functions to initialise queues and check physical sources
C
      CALL EQXBIN ( BRKPHS )
      CALL EQXKIN ( KBDPHS )
      CALL EQXPIN ( PNTPHS )
      CALL EQXSIN ( SWTPHS )
C
C     Now physically open the queue sources if they physically exist
C
      STATUS = .FALSE.
      IF ( BRKPHS ) THEN
         STATUS = EQXBOP ( 1 )
      ENDIF
      IF ( .NOT. STATUS ) THEN
         IF ( KBDPHS ) THEN
            STATUS = EQXKOP ( 1 )
         ENDIF
         IF ( .NOT. STATUS ) THEN
            IF ( PNTPHS ) THEN
               STATUS = EQXPOP ( 1 )
            ENDIF
            IF ( .NOT. STATUS ) THEN
               IF ( SWTPHS ) THEN
                  STATUS = EQXSOP ( 1 )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     All done
C
      EQINIT = STATUS
C
      RETURN
      IDUMMY = DUMMY
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
