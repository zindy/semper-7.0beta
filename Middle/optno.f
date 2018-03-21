C Semper 6 system module OPTNO
C
      LOGICAL FUNCTION OPTNO(NAME)
      INTEGER NAME
C
C Returns .TRUE. if variable NAME is set and zero
C
      LOGICAL SEMLU
      REAL VALUE
C
      OPTNO = .FALSE.
      IF (SEMLU(-1,NAME,VALUE)) OPTNO = VALUE .EQ. 0.0
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
