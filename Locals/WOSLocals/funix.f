C Semper 6 processing module FUNIX
C
      SUBROUTINE FUNIX
C
C Executes Unix command string.
C
C Syntax:   Unix :FUNIX $1='
C
      INTEGER IPACK
      LOGICAL SEMKTX,CUNIX
C
      INTEGER LENCOM
      PARAMETER ( LENCOM = 200 )
C
      INTEGER TEXT(LENCOM),N
      CHARACTER*(LENCOM) COMMAND
C
      INCLUDE 'COMMON'
C
C Fetch Unix command string from the command line
C
   10 N = LENCOM
      IF (SEMKTX(-12441,'Unix command (as textstring): ',
     +           TEXT,N,.FALSE.)) RETURN
C
      IF (N.EQ.0) GOTO 10
C
C Copy the Unix command string into a Fortran character string
C
      CALL SEMCHS(COMMAND(1:N),TEXT,N)
C
C Execute the Unix command string
C
      IF (CUNIX(COMMAND(1:N))) THEN
C
C Pass through break and page error
C
         IF (ERROR.NE.BRKERR.AND.ERROR.NE.PAGERR) THEN
            ERROR = 77
            IDMESS =
     +         'Error detected while trying to execute Unix command'
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1994:  Synoptics Ltd,  All Rights Reserved
C
      END
