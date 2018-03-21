C Semper 6 system module IVAL
C
      INTEGER FUNCTION IVAL(NAME)
C
C Returns (nearest integer to) value of Semper variable NAME
C
      INTEGER NAME
C
      LOGICAL SEMLU
C
      REAL X
C
C Note that SEMLU sets X to 0.0 if variable is not set
C
      IF (SEMLU(-1,NAME,X)) CONTINUE
      IVAL = NINT(X)
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
