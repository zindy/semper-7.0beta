C Semper 6 system module SEMINT
C
      LOGICAL FUNCTION SEMINT(NOPROG)
C
C Returns .FALSE. if UIF is running or in BATCH mode
C If NOPROG is set then will also return .FALSE. if using a run
C file or program library
C
      LOGICAL NOPROG
C
      INCLUDE 'COMMON'
C
      SEMINT = .FALSE.
C
C Fault batch or UIF input
C
      IF (BATCH .EQ. 0.0 .AND. UIF .EQ. 0.0) THEN
         IF (NOPROG) THEN
C
C Fault RUN file or program library
C
            IF (INPUT .EQ. TERM1 .AND. INPLEV .EQ. 0) SEMINT = .TRUE.
         ELSE
            SEMINT = .TRUE.
         ENDIF
      ENDIF
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
