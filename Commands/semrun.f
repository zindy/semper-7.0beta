C Semper 6 processing module SEMRUN
C
      SUBROUTINE SEMRUN
C
      LOGICAL FILSEA,FILSTR,FORTRD,SEMIOE
C
C Attaches a RUN file to INPUT
C
      INCLUDE 'COMMON'
C
      INTEGER IOS,N
C
      CHARACTER*(FILMAX) FILENM,PATHNM
      CHARACTER*4 DFNAM
C
      LOGICAL FOUND
C
      IF (INPLEV .NE. 0) THEN
         ERROR = 107
      ELSE IF (INPUT .EQ. RUNFLE) THEN
         ERROR = 24
      ELSE IF (FORLEV .NE. 0) THEN
         ERROR = 107
      ELSE
C
C Read name from BUFFER and attach to RUNFLE
C
         DFNAM = '.run'
         CALL INPDEF(DFNAM)
         IF (FILSTR(' ',FILENM,N,.FALSE.)) GOTO 10
         IF (N .EQ. 0) GOTO 10
C
         IF (FILSEA(FILENM,DFNAM,PATHNM,FOUND)) GOTO 10
         IF (FOUND) THEN
            IF (.NOT.FORTRD(RUNFLE,PATHNM,IOS)) THEN
C
C Set new input unit and cancel rest of RUN line?!
C
               INPUT = RUNFLE
               LINLEN = 0
               GOTO 10
            ENDIF
            IF (SEMIOE(IOS,RUNFLE,PATHNM)) GOTO 10
         ELSE
            ERROR = 130
            IDMESS = FILENM(1:N)
         ENDIF
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1988-1996 : Synoptics Ltd,  All Rights Reserved
C
      END
