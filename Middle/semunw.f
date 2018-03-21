C Semper 6 system module SEMUNW
C
      LOGICAL FUNCTION SEMUNW(LEVEL)
C
C Unwinds local variable table to LEVEL
C
      INTEGER LEVEL
      LOGICAL SEMLU,SEMDIA,FAULT
      REAL    X
C
      INCLUDE 'COMMON'
C
      SEMUNW = .TRUE.
C
      FAULT = .FALSE.
C
   10 IF (LOCLEV .GT. LEVEL) THEN
C
         IF (LOCUNS(LOCLEV)) THEN
            IF (SEMLU(0,LOCVAR(LOCLEV),X)) FAULT = .TRUE.
         ELSE
            IF (SEMLU(1,LOCVAR(LOCLEV),LOCVAL(LOCLEV))) FAULT = .TRUE.
         ENDIF
C
         LOCLEV = LOCLEV - 1
C
         GOTO 10
      ENDIF
C
C Bring verb locals down if over current limit (error recovery)
C
      IF (VRBLEV .GT. LOCLEV) VRBLEV = LOCLEV
C
      IF (FAULT) THEN
         ERROR=0
         IF (SEMDIA('Warning: unable to restore local variable(s)',
     +      NDIWAR)) GOTO 20
      ENDIF
C
      SEMUNW = .FALSE.
C
   20 RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
