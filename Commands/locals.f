C Semper 6 processing module LOCALS
C---------------------------------------------------------------------
C
C      SUBROUTINE LOCALS
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command INKEY, LOCAL and UNSET
C
C---------------------------------------------------------------------
C
      SUBROUTINE LOCALS
C
C     =================
C
      INTEGER IPACK,IVAL
      LOGICAL SEMLU,SEMXA1
      LOGICAL INKEY
C
      INCLUDE 'COMMON'
C
      REAL X
      INTEGER CH,IPTR,NAME
C
C Any arguments given
C
      IPTR = IVAL(-11201)
      IF (IPTR .EQ. 0) THEN
C
C No arguments - is there a default action?
C
         IF (VERB .EQ. 14971) THEN
C
C For INKEY, swallow a key press and go away
C
            IF (INKEY(CH,ERROR)) GOTO 20
         ENDIF
         GOTO 20
      ENDIF
C
C Next item
C
   10 IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) CONTINUE
      IF (ERROR .NE. 0) GOTO 20
C
C Fault bad name
C
      IF (NAME .EQ. 0) GOTO 30
C
C Fault bad terminator character (only space or comma allowed)
C
      IF (IPTR .LE. COMLIM) THEN
         CH = LINBUF(IPTR)
         IF (.NOT. (CH .EQ. KSPACE .OR. CH .EQ. KCOMMA)) GOTO 30
      ENDIF
C
C LOCAL processing
C
      IF (VERB .EQ. 19803) THEN
C
C LOCAL not allowed in FOR
C
         IF (FORLEV .GT. INPFOR(INPLEV)) THEN
            ERROR = 103
            GOTO 20
         ENDIF
C
C Set variable as local to this level
C
         VRBLEV = INPLOC(INPLEV)
         IF (SEMLU(3,NAME,X)) GOTO 20
         VRBLEV = LOCLEV
C
C UNSET variable
C
      ELSE IF (VERB .EQ. -2180) THEN
         IF (SEMLU(0,NAME,X)) GOTO 20
C
C INKEY processing
C
      ELSE IF (VERB .EQ. 14971) THEN
         IF (INKEY(CH,ERROR)) GOTO 20
C
C Set variable to key code
C
         IF (SEMLU(1,NAME,REAL(CH))) GOTO 20
      ENDIF
C
      IPTR = IPTR + 1
      IF (IPTR .LE. COMLIM) GOTO 10
C
   20 RETURN
C
C Bad syntax
C
   30 ERROR = 17
      GOTO 20
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
