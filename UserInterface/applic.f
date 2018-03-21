C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION APPGET ( NAME, VALUE )
C       -----------------------------------------------
C
C       PARAMETERS:
C
C       integer name : INPUT - the packed name of the variable to get
C
C       real value : OUTPUT - the value assigned to the variable
C
C       Gets the value of the application variable given to the given
C       value.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION APPGET ( NAME, VALUE )
C     =======================================
C
      INTEGER NAME
      REAL VALUE
C
C     CALLED FUNCTIONS:
C
C     Gets/sets the value of a Semper variable
C
      LOGICAL SEMLU
C
C     Get the value of the variable
C
      APPGET = SEMLU ( -1, NAME, VALUE )
      APPGET = .FALSE.
C
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION APPACK ( NAME, VALUE )
C       ---------------------------------------
C
C       PARAMETERS:
C
C       integer name : INPUT - packed name of the variable to set
C
C       real value : INPUT - the value to assign to the variable
C
C       Sets the value of the application variable given to the given
C       value.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION APPACK ( NAME, VALUE )
C     =======================================
C
      INTEGER NAME
      REAL VALUE
C
C     CALLED FUNCTIONS:
C
      LOGICAL SEMLU
C
C     Set the value of the variable
C
      APPACK = SEMLU ( 1, NAME, VALUE )
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION APPSET ( NAME, LENGTH, VALUE )
C       -----------------------------------------------
C
C       PARAMETERS:
C
C       character*(*) name : INPUT - the name of the variable to set
C
C       integer length : INPUT - the length of the variable name
C
C       real value : INPUT - the value to assign to the variable
C
C       Sets the value of the application variable given to the given
C       value.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION APPSET ( NAME, LENGTH, VALUE )
C     ===============================================
C
      CHARACTER*(*) NAME
      INTEGER LENGTH
      REAL VALUE
C
C     LOCAL VARIABLES:
C
C     Packed version of the variable name
C
      INTEGER INAME
C
C     CALLED FUNCTIONS:
C
C     Returns a packed name
C
      INTEGER IPACK
C
C     Sets the value of a packed variable name
C
      LOGICAL APPACK
C
C     Convert the character string to a packed name
C
      INAME = IPACK ( NAME(1:LENGTH) )
C
C     Set the value of the variable
C
      APPSET = APPACK ( INAME, VALUE )
C
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION APPVAL ( STRING, LENGTH, VALUE )
C       -------------------------------------------------
C
C       PARAMETERS:
C
C       character*(*) string : INPUT - the string to be evaluated
C
C       integer length : INPUT - the length of the string
C
C       real value : OUTPUT - the value of the expression represented
C                    by the string.
C
C       Executes the expression given in the string, and returns the
C       value of that evaluation.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION APPVAL ( STRING, LENGTH, VALUE )
C     =================================================
C
      CHARACTER*(*) STRING
      INTEGER LENGTH
      REAL VALUE
C
C     LOCAL VARIABLES:
C
C     Integer form of the string
C
      INTEGER IBUFF(80)
C
C     Length of the buffer (local copy)
C
      INTEGER LEN
C
C     Loop counter
C
      INTEGER I
C
C     Pointer into the string at which to start evaluation
C
      INTEGER IPTR
C
C     CALLED FUNCTIONS:
C
C     Evaluates an expression
C
      LOGICAL SEMEXP
C
C     Copy the string variable into an integer array for Semper
C
      LEN = LENGTH
      IF ( LENGTH .GT. 80 ) LEN = 80
      DO 10 I = 1, LEN
         IBUFF(I) = ICHAR ( STRING(I:I) )
   10 CONTINUE
C
C     Now evaluate it
C
      IPTR = 1
      APPVAL = SEMEXP ( IBUFF, LEN, IPTR, VALUE, .false. )
C
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION APPSTR ( VALUE, STRING, LENGTH, RETLEN )
C       ---------------------------------------------------------
C
C       PARAMETERS:
C
C       real value : INPUT - the value to be returned as a string.
C
C       character*(*) string : OUTPUT - the string representing the
C                              value
C
C       integer length : INPUT - the length of the string variable
C
C       integer retlen : OUTPUT - the length of the return data in the
C                        string
C
C       Converts the numeric value given to a string.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION APPSTR ( VALUE, STRING, LENGTH, RETLEN )
C     =========================================================
C
      REAL VALUE
      CHARACTER*(*) STRING
      INTEGER LENGTH
      INTEGER RETLEN
C
C     LOCAL VARIABLES:
C
C     Integer form of the string
C
      INTEGER IBUFF(80)
C
C     Loop counter
C
      INTEGER I
C
C     Pointer into the buffer at which to start the conversion, and
C     length of result
C
      INTEGER IPTR
C
C     Dummy variable
C
      INTEGER IVALUE
C
C     CALLED FUNCTIONS:
C
C     Converts a real value to a string
C
      LOGICAL SEMXA1
C
C     Do the conversion into ibuff
C
      IPTR = 1
      APPSTR = SEMXA1 ( 4, IBUFF, LENGTH, IPTR, VALUE, IVALUE )
      IF ( .NOT. APPSTR ) THEN
C
C        Now copy into string
C
         RETLEN = IPTR - 1
         IF ( RETLEN .GT. LENGTH ) RETLEN = LENGTH
         DO 10 I = 1, RETLEN
            STRING (I:I) = CHAR( IBUFF(I))
   10    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE APPWAI ( SECS )
C       --------------------------
C
C       PARAMETERS:
C
C       real secs : INPUT - the number of seconds to wait
C
C       Waits for the given number of seconds.
C
C----------------------------------------------------------------------
C
      SUBROUTINE APPWAI ( SECS )
C     ==========================
C
      REAL SECS
C
      CALL WAITS ( SECS )
C
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       SUBROUTINE APPBEL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Ring the bell
C
C----------------------------------------------------------------------
C
      SUBROUTINE APPBEL
C     =================
C
      LOGICAL SEMBEE
C
      INCLUDE 'COMMON'
C
      DUMLOG = SEMBEE()
C
      RETURN
C
C Copyright (C) 1988, 1989, 1991 :  Synoptics Ltd,  All Rights Reserved
C
      END
