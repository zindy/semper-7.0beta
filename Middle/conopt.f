C Semper 6 system module CONOPT
C
      LOGICAL FUNCTION CONOPT(NOPT1,NOPT2)
      INTEGER NOPT1,NOPT2
C
C Faults conflicting options NOPT1 and NOPT2
C
      LOGICAL OPT
C
      INCLUDE 'COMMON'
C
C See if options NOPT1 and NOPT2 conflict
C
      CONOPT = .FALSE.
      IF (OPT(NOPT1)) THEN
         IF (OPT(NOPT2)) THEN
            CONOPT = .TRUE.
C
C Conflict does exist, set corresponding error parameters
C
            ERROR  = 60
            IDERR  = NOPT1
            IDERR2 = NOPT2
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
