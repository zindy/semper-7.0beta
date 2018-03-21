C----------------------------------------------------------------------
C
C       SUBROUTINE EQXSIN ( EXISTS )
C       ----------------------------
C
C       PARAMETERS:
C
C       logical exists : OUTPUT - True if switches source physically
C                        exists
C
C       Function is used to initialise the switches event queue data
C       structures.  Common data is initialised, and the presence of
C       the physical source for breaks is checked.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXSIN ( EXISTS )
C     ============================
C
      LOGICAL EXISTS
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQXCOM'
      INCLUDE 'EQCOM'
C
C     Initialise queue data
C
      SWTSTA = QCLOSE
      SQINTO = 0
      SQREAD = 0
C
C     Say mouse is there.
C
      EXISTS = .TRUE.
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXSDA ( I1, I2, I3 )
C       --------------------------------
C
C       PARAMETERS:
C
C       integer i1, i2, i3 : OUTPUT - Information on the queue.
C
C       Function is used to return data on the switches event queue.
C       I1, I2 and I3 are used to return information about the event
C       queue: I1 returns the number of switches.  I2 is returned as
C       zero if switch closure cannot be detected, otherwise 1.  I3
C       returns a similar value to indicate the ability to detect
C       switch opening.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXSDA ( I1, I2, I3 )
C     ================================
C
      INTEGER I1, I2, I3
C
C     Tell caller about number of buttons (can only guess!),
C     switch opening and closing detection
C
      I1 = 3
      I2 = 1
      I3 = 1
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXSPL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Function is used to poll on the switches queue.
C       Sends a poll request to X-Windows.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXSPL
C     =================
C
      CALL SX11CE
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXSOP ( QUESET )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer queset : INPUT - the queue number for multiple queues
C
C       Function is used to open the switches event queue.  Performs
C       any opening and initialisation needed on the switches device.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXSOP ( QUESET )
C     ==================================
C
      INTEGER QUESET
C
      EQXSOP = .FALSE.
      IDUMMY = QUESET

      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXSCL ( QUESET )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer queset : INPUT - the queue number for multiple queues
C
C       Function is used to close the switches event queue.  Performs
C       any read termination on the switches device, and then any
C       closing action.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXSCL ( QUESET )
C     ==================================
C
      INTEGER QUESET
C
      EQXSCL = .FALSE.
      IDUMMY = QUESET
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXSFL
C       -----------------
C
C       PARAMETERS:
C
C       None
C
C       Flushes the physical switches queue if one exists, else does
C       nothing.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXSFL
C     =================
C
C     All done
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXXSE ( SWITCH, OPENED )
C       ------------------------------------
C
C       PARAMETERS:
C
C       integer switch : INPUT - Switch number which has changed.
C
C       logical opened : INPUT - True if switch opened, else False.
C
C       Enters a switch state change into the queue if there is space,
C       the queue state is QRUN or QWAIT, and the queue is not disabled.
C       The queue write pointer is updated.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXXSE ( SWITCH, OPENED )
C     ====================================
C
      INTEGER SWITCH
      LOGICAL OPENED
C
      INCLUDE 'EQCOM'
      INCLUDE 'EVENTS'
C
C     LOCAL VARIABLES:
C
C     Mask for setting switch closure set
C
      INTEGER CLMASK
C
C     Queue entry to fill
C
      INTEGER IND
C
C     Record last state
C
      IF ( OPENED ) THEN
         SWTLOP = SWITCH
         SWTLCL = 0
C        Changed from LSHIFT to ISHFT
         CLMASK = ISHFT( 1, SWITCH - 1)
C         SWTLCS = XOR ( CLMASK, SWTLCS )
C        Changed from XOR to IEOR
         SWTLCS = IEOR ( CLMASK, SWTLCS )
      ELSE
         SWTLOP = 0
         SWTLCL = SWITCH
C        Similar changes to above
         CLMASK = ISHFT( 1, SWITCH - 1)
         SWTLCS = IEOR ( CLMASK, SWTLCS )
      ENDIF
C
C     Check queue is in correct state, not disabled and not full
C
      IND = IAND ( SQINTO + 1, SQMASK )
      IF ( (SWTSTA .EQ. QRUN .OR. SWTSTA .EQ. QWAIT) .AND.
     +     (.NOT. SQDIS) .AND. (IND .NE. SQREAD) ) THEN
C
C        Put data into queue
C
         SQINTO = IND
         SQUEUE(1,SQINTO) = SWTLCL
         SQUEUE(2,SQINTO) = SWTLOP
         SQUEUE(3,SQINTO) = SWTLCS
      ENDIF
C
C     All done
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
