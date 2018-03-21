C----------------------------------------------------------------------
C
C       SUBROUTINE EQXBIN ( EXISTS )
C       ----------------------------
C
C       PARAMETERS:
C
C       logical exists : OUTPUT - True if break source physically
C                        exists
C
C       Function is used to initialise the break event queue data
C       structures.  Common data is initialised, and the presence of
C       the physical source for breaks is checked.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXBIN ( EXISTS )
C     ============================
C
      LOGICAL EXISTS
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQXCOM'
      INCLUDE 'EQCOM'
C
C     Source of breaks (keyboard) is there always.
C
      EXISTS = .TRUE.
C
C     Initialise queue data
C
      BRKSTA = QCLOSE
      BRKCNT = 0
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXBDA ( I1, I2, I3 )
C       --------------------------------
C
C       PARAMETERS:
C
C       integer i1, i2, i3 : OUTPUT - Information from initialisation
C
C       Function is used to return data on the break event queue.
C       I1 is used to return information about the event queue: it will
C       be returned as zero if break cannot be trapped, otherwise 1.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXBDA ( I1, I2, I3 )
C     ================================
C
      INTEGER I1, I2, I3
C
C     First, tell caller that we can catch breaks for them
C
      I1 = 1
C     To stop gfortran complaining
      IDEBUG=I2
      IDEBUG=I3
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXBPL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Function is used to poll the break queue.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXBPL
C     =================
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     Call the function to poll the keyboard, as breaks will not be
C     trapped by the signal handler if the keyboard queue is running.
C
      IF ( KEYSTA .EQ. QRUN .OR. KEYSTA .EQ. QWAIT ) THEN
          CALL EQXXPK
      ENDIF
C
      RETURN
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXBOP ( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy variable
C
C       Function opens the break queue.  A channel is opened to the
C       terminal, if one is not already open, and the break AST handler
C       is set up.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXBOP ( DUMMY )
C     =================================
C
      INTEGER DUMMY
C
      INCLUDE 'EQXCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
      STATUS = .FALSE.
C
C     Set up a break signal handler.
C
      CALL EQXXBO
C
C     All done
C
      EQXBOP = STATUS
      IDEBUG=DUMMY      ! To satisfy gfortran
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXBCL ( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy variable
C
C       Function is used to close down the break event queue.  The
C       channel to the terminal is deassigned.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXBCL ( DUMMY )
C     =================================
C
      INTEGER DUMMY
C
      INCLUDE 'EQXCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
      STATUS = .FALSE.
C
C     Remove the break handler.
C
      CALL EQXXBC
C
C     All done
C
      EQXBCL = STATUS
      IDEBUG=DUMMY      ! To satisfy gfortran
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXXBE
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       This routine is to insert a break into the break queue.  The
C       break queue state is examined, and if it is QRUN or QWAIT, and
C       the queue is not disabled, then the break count is incremented.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXXBE
C     =================
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     Check state
C
      IF ( (BRKSTA .EQ. QRUN .OR. BRKSTA .EQ. QWAIT) .AND.
     +     (.NOT. BQDIS) ) THEN
         BRKCNT = BRKCNT + 1
      ENDIF
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
