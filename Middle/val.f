C Semper 6 system module VAL
C
      REAL FUNCTION VAL(NAME)
C
C Returns value of Semper variable NAME
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
      VAL = X
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
