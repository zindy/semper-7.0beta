C----------------------------------------------------------------------
C
C       SUBROUTINE EQKBIN ( EXISTS )
C       ----------------------------
C
C       PARAMETERS:
C
C       logical exists : OUTPUT - True if keyboard source physically
C                        exists
C
C       Function is used to initialise the keyboard event queue data
C       structures.  Common data is initialised, and the presence of
C       the physical source for breaks is checked.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXKIN ( EXISTS )
C     ============================
C
      LOGICAL EXISTS
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQXCOM'
      INCLUDE 'EQCOM'
C
C     Source (keyboard) is there always.
C
      EXISTS = .TRUE.
C
C     Initialise queue data
C
      KEYSTA = QCLOSE
      KQINTO = 0
      KQREAD = 0
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXKDA ( I1, I2, I3 )
C       --------------------------------
C
C       PARAMETERS:
C
C       integer i1, i2, i3 : OUTPUT - Information from initialisation
C
C       Function is used to return data on the keyboard event queue.
C       I1, I2 and I3 are used to return information about the event
C       queue: I1 and I2 return the min. and max. function key numbers
C       detectable, I3 it will be returned as zero if no cursor keys
C       are detectable, otherwise 1
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXKDA ( I1, I2, I3 )
C     ================================
C
      INTEGER I1, I2, I3
C
C     Tell caller about the function keys we can deal with
C
      I3 = 0
      IDUMMY=I1
      IDUMMY=I2
      RETURN
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXKPL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Function is used to poll the keyboard queue.  If a key has been
C       pressed, it is translated into an internal code, and queued if
C       the queue is in a suitable state.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXKPL
C     =================
C
C     Poll the keyboard.
C
      CALL EQXXPK
C
      RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXKOP( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy variable
C
C       Function is used to open the keyboard event queue.  Any
C       necessary opening and reading initialisation is performed.
C
C       Function returns .TRUE. in case of error, otherwise .FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXKOP ( DUMMY )
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
C     Open up the keyboard for input
C
      CALL EQXXKO
C
C     All done
C
      EQXKOP = STATUS
      IDUMMY = DUMMY

      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXKCL ( DUMMY )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - Dummy variable
C
C       Function is used to close the keyboard event queue.  Any
C       closing and read diabling are carried out.
C
C       Function returns .TRUE. in case of error, otherwise .FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXKCL ( DUMMY )
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
C     Close down the keyboard.  Breaks now handled by signal.
C
      CALL EQXXKC
C
C     All done
C
      EQXKCL = STATUS
      IDUMMY = DUMMY
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE EQXXKE ( INTCH )
C       ---------------------------
C
C       PARAMETERS:
C
C       integer intch : INPUT - The integer coded character to insert
C                       into the queue.
C
C       Loads a single character into the keyboard event queue.  Will
C       only enter into the queue if there is space, and the queue
C       state is QRUN or QWAIT, and the queue is not disabled.  The
C       queue write pointer is updated.
C
C----------------------------------------------------------------------
C
      SUBROUTINE EQXXKE ( INTCH )
C     ===========================
C
      INTEGER INTCH
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Write pointer into queue we are about to use
C
      INTEGER IND
C
C     Do nothing if not in suitable state, or queue full.
C
      KQLAST = INTCH
      IND = IAND ( KQINTO + 1, KQMASK )
      IF ( (KEYSTA .EQ. QRUN .OR. KEYSTA .EQ. QWAIT) .AND.
     +     (.NOT. KQDIS) .AND. (IND .NE. KQREAD) ) THEN
C
C        Insert into the queue
C
         KQINTO = IND
         KQUEUE(KQINTO) = INTCH
      ENDIF
C
C     All done
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
