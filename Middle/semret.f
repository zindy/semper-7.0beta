C Semper 6 system module SEMRET
C
      LOGICAL FUNCTION SEMRET(LEVEL)
C
      INTEGER LEVEL
      LOGICAL SEMENV,SEMUNW
C
C Returns input to activation level LEVEL
C
      INCLUDE 'COMMON'
C
      SEMRET = .TRUE.
C
      IF (INPLEV .EQ. 0) THEN
         IF (INPUT .EQ. RUNFLE) THEN
            CLOSE(RUNFLE,ERR=10)
   10       INPUT = TERM1
            LINLEN = 0
            LINPTR = 0
         ENDIF
         SEMRET = .FALSE.
      ELSE
   20    IF (LEVEL .LT. INPLEV) THEN
C
            IF (SEMUNW(INPLOC(INPLEV))) GOTO 30
            FORLEV = INPFOR(INPLEV)
            INPLEV = INPLEV - 1
            NEXTSC = INPNXT(INPLEV)
            LASTSC = NEXTSC
            TRAP = INTRAP(INPLEV)
C
C Note that SEMENV may reset LINPTR as well
C
            LINPTR = INPLEN(INPLEV)
            GOTO 20
         ENDIF
C
C Now restore environment line
C
         SEMRET = SEMENV(INPDEV(INPLEV),INPSLT(INPLEV),INPLIN(INPLEV))
      ENDIF
C
   30 RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
