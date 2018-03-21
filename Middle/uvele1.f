C       LOGICAL FUNCTION UVELE1 ( TYPE, PID, EID )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer type : INPUT - the type of element we are creating.
C
C       integer pid : OUTPUT - the panel on which the element resides.
C
C       integer eid : OUTPUT - the identifier of the element.
C
C       Implements the UIF general ELEMENT keys and options CREATE,
C       ID and IN.  Returns and acts on the data set for these keys and
C       options which are available for all elements.
C
C       Function returns TRUE in case of error, oterwise FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UVELE1 ( TYPE, PID, EID )
C     ==========================================
C
      INTEGER TYPE, PID, EID
C
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Function return status
C
      LOGICAL STATUS
C
C     Real value of a key
C
      REAL VALUE
C
C     Packed names of keys and options
C
      INTEGER CREATE,ID,IN
C
      PARAMETER (CREATE=5525,ID=14560,IN=14960)
C
C     CALLED FUNCTIONS:
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Reads a key as an integer
C
      INTEGER IVAL
C
C     Creates a element
C
      LOGICAL UIFECR
C
C     Checks to see is a variable/key is set and returns value
C
      LOGICAL SEMLU
C
C     Get the identifier of the element to work on: may involve
C     creating a new one
C
      IF ( OPT ( CREATE ) ) THEN
C
C        Create a new element.  Get the panel to create it on.
C
         PID = IVAL ( IN )
         STATUS = UIFECR ( PID, TYPE, EID )
      ELSE IF ( SEMLU ( -1, ID, VALUE ) ) THEN
C
C        Get the element id.
C
         EID = INT ( VALUE )
         STATUS = .FALSE.
      ENDIF
C
      IF ( STATUS ) CALL UIFAPE
C
C     All done.
C
      UVELE1 = STATUS
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
