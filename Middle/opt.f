C Semper 6 system module OPT
C
      LOGICAL FUNCTION OPT(NAME)
      INTEGER NAME
C
C Returns .TRUE. if variable NAME is set and non-zero
C
      LOGICAL SEMLU
      REAL VALUE
C
      OPT = .FALSE.
      IF (SEMLU(-1,NAME,VALUE)) OPT = VALUE .NE. 0.0
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
