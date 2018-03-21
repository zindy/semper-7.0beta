C----------------------------------------------------------------------
C
C       SUBROUTINE EQXPIN ( EXISTS )
C       ----------------------------
C
C       PARAMETERS:
C
C       logical exists : OUTPUT - True if pointer source physically
C                        exists
C
C       Function is used to initialise the pointer event queue data
C       structures.  Common data is initialised, and the presence of
C       the physical source for breaks is checked.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXPIN ( EXISTS )
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
      PNTSTA = QCLOSE
      PQINTO = 0
      PQREAD = 0
      PNTXLD = 0
      PNTYLD = 0
      EXISTS = .TRUE.
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXPDA ( I1, I2, I3 )
C       --------------------------------
C
C       PARAMETERS:
C
C       integer i1, i2, i3 : OUTPUT - Information from initialisation
C
C       Function is used to return data on the pointer event queue.
C       I1 is used to return a value indicating whether the pointer
C       is echoed by hardware.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXPDA ( I1, I2, I3 )
C     ================================
C
      INTEGER I1, I2, I3
C
      INCLUDE 'EVENTS'
C
C     Tell caller about echo information
C
      I1 = ECHALL
C
      IDUMMY = I2
      IDUMMY = I3
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXPPL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Function is used to initialise polling on the pointer queue.
C       Sends a poll request to the mouse, the result of which is
C       interpreted for both pointer and switches queues.  Note that
C       this function may be called before the mouse channel is
C       assigned, so this must be guarded against.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXPPL
C     =================
C
C     Poll the mouse - don't need to!
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXPOP ( QUESET )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer queset : INPUT - queue number in multiple systems
C
C       Function is used to open the pointer event queue.  Performs any
C       opening and read initialisation on the mouse.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXPOP ( QUESET )
C     ==================================
C
      INTEGER QUESET
C
C     Call function to enable the input queue to read pointer
C     data.
C
      CALL EQXXPO
C
C     All done
C
      EQXPOP = .FALSE.
      IDUMMY=QUESET
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXPCL ( QUESET )
C       ----------------------------------
C
C       PARAMETERS:
C
C       integer queset : INPUT - Queue number in multiple environment
C
C       Function is used to close the pointer event queue.  Terminates
C       any reads on the mouse, and performs any closing needed on the
C       mouse.
C
C       Function returns TRUE in the case of error, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXPCL ( QUESET )
C     ==================================
C
      INTEGER QUESET
C
C     Call function to disable the input queue reading pointer
C     data.
C
      CALL EQXXPC
C
      EQXPCL = .FALSE.
      IDUMMY=QUESET
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXPFL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Flushes the physical pointer queue if one exists, else does
C       nothing.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXPFL
C     =================
C
C     All done
C
      RETURN
C
C Copyright (C) 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXXPE ( DELTAX, DELTAY )
C       ------------------------------------
C
C       PARAMETERS:
C
C       integer deltax, deltay : INPUT - Point move deltas.
C
C       Enters a new pointer delta into the queue.  Will only enter
C       into the queue if there is space, the queue state is QRUN or
C       QWAIT, and the queue is not disabled.  The queue write pointer
C       is updated.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXXPE ( DELTAX, DELTAY )
C     ====================================
C
      INTEGER DELTAX, DELTAY
C
      INCLUDE 'PARAMS'
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Queue entry to fill
C
      INTEGER IND
C
C     Check queue is in correct state, not disabled and not full
C
      PNTXLD = DELTAX
      PNTYLD = DELTAY
      IND = IAND ( PQINTO + 1, PQMASK )
      IF ( (PNTSTA .EQ. QRUN .OR. PNTSTA .EQ. QWAIT) .AND.
     +     (.NOT. PQDIS) .AND. (IND .NE. PQREAD) ) THEN
C
C        Increment write pointer
C
         PQINTO = IND
C
C        And save in queue
C
         PQUEUE(1,PQINTO) = DELTAX
         PQUEUE(2,PQINTO) = DELTAY
      ENDIF
C
C     All done
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
